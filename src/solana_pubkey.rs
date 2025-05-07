use borsh::{BorshDeserialize, BorshSerialize};
use core::fmt;

/// Convenience macro to define a static public key.
///
/// Input: a single literal base58 string representation of a Pubkey.
#[macro_export]
macro_rules! pubkey {
    ($input:literal) => {
        $crate::Pubkey::from_str_const($input)
    };
}

#[doc(hidden)]
pub mod pubkey {}

/// Maximum string length of a base58 encoded pubkey
const MAX_BASE58_LEN: usize = 44;

fn write_as_base58(f: &mut fmt::Formatter, p: &Pubkey) -> fmt::Result {
    let mut out = [0u8; MAX_BASE58_LEN];
    let out_slice: &mut [u8] = &mut out;

    // This will never fail because the only possible error is BufferTooSmall,
    // and we will never call it with too small a buffer.

    let len = bs58::encode(p.0).onto(out_slice).unwrap();
    let as_str = core::str::from_utf8(&out[..len]).unwrap();

    f.write_str(as_str)
}

/// Number of bytes in a pubkey
const PUBKEY_BYTES: usize = 32;

/// maximum length of derived `Pubkey` seed
const MAX_SEED_LEN: usize = 32;

/// Maximum number of seeds
pub const MAX_SEEDS: usize = 16;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PubkeyError {
    /// Length of the seed is too long for address generation
    MaxSeedLengthExceeded,
    InvalidSeeds,
    IllegalOwner,
}

/// The address of a [Solana account][acc].
///
/// Some account addresses are [ed25519] public keys, with corresponding secret
/// keys that are managed off-chain. Often, though, account addresses do not
/// have corresponding secret keys &mdash; as with [_program derived
/// addresses_][pdas] &mdash; or the secret key is not relevant to the operation
/// of a program, and may have even been disposed of.
///
/// [acc]: https://solana.com/docs/core/accounts
/// [ed25519]: https://ed25519.cr.yp.to/
/// [pdas]: https://solana.com/docs/core/cpi#program-derived-addresses
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(
    Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd, BorshDeserialize, BorshSerialize,
)]
pub struct Pubkey([u8; PUBKEY_BYTES]);

impl fmt::Debug for Pubkey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_as_base58(f, self)
    }
}

impl fmt::Display for Pubkey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write_as_base58(f, self)
    }
}

impl Pubkey {
    pub const fn new_from_array(pubkey_array: [u8; 32]) -> Self {
        Self(pubkey_array)
    }

    /// Decode a string into a Pubkey, usable in a const context
    pub const fn from_str_const(s: &str) -> Self {
        let id_array = five8_const::decode_32_const(s);
        Pubkey::new_from_array(id_array)
    }

    /// Find a valid program derived address and its corresponding bump seed.
    pub const fn find_program_address(seeds: &[&[u8]], program_id: &Pubkey) -> (Pubkey, u8) {
        Self::try_find_program_address(seeds, program_id)
            .expect("Unable to find a viable program address bump seed")
    }

    pub const fn try_find_program_address(
        seeds: &[&[u8]],
        program_id: &Pubkey,
    ) -> Option<(Pubkey, u8)> {
        let mut bump_seed = [u8::MAX];
        loop {
            match Self::create_program_address_helper(seeds, &[&bump_seed], program_id) {
                Ok(address) => return Some((address, bump_seed[0])),
                Err(PubkeyError::InvalidSeeds) if bump_seed[0] != 0 => (),
                _ => return None,
            }
            bump_seed[0] -= 1;
        }
    }

    pub const fn to_bytes(self) -> [u8; 32] {
        self.0
    }

    /// Return a reference to the `Pubkey`'s byte array.
    #[inline(always)]
    pub const fn as_array(&self) -> &[u8; 32] {
        &self.0
    }

    pub const fn create_program_address(
        seeds: &[&[u8]],
        program_id: &Pubkey,
    ) -> Result<Self, PubkeyError> {
        Self::create_program_address_helper(seeds, &[], program_id)
    }
    const fn create_program_address_helper(
        seeds1: &[&[u8]],
        seeds2: &[&[u8]],
        program_id: &Pubkey,
    ) -> Result<Self, PubkeyError> {
        if seeds1.len() + seeds2.len() > MAX_SEEDS {
            return Err(PubkeyError::MaxSeedLengthExceeded);
        }

        {
            let mut i = 0;
            while i < seeds1.len() {
                if seeds1[i].len() > MAX_SEED_LEN {
                    return Err(PubkeyError::MaxSeedLengthExceeded);
                }
                i += 1;
            }
        }
        {
            let mut i = 0;
            while i < seeds2.len() {
                if seeds2[i].len() > MAX_SEED_LEN {
                    return Err(PubkeyError::MaxSeedLengthExceeded);
                }
                i += 1;
            }
        }

        const PDA_MARKER: &[u8; 21] = b"ProgramDerivedAddress";
        let mut hasher = lhash::Sha256::new();

        {
            let mut i = 0;
            while i < seeds1.len() {
                hasher = hasher.const_update(seeds1[i]);
                i += 1;
            }
        }
        {
            let mut i = 0;
            while i < seeds2.len() {
                hasher = hasher.const_update(seeds2[i]);
                i += 1;
            }
        }
        hasher = hasher.const_update(&program_id.0);
        hasher = hasher.const_update(PDA_MARKER);

        let hash = hasher.const_result();

        if bytes_are_curve_point(hash) {
            return Err(PubkeyError::InvalidSeeds);
        }

        Ok(Pubkey::new_from_array(hash))
    }
}

pub const fn bytes_are_curve_point(bytes: [u8; 32]) -> bool {
    #[derive(Copy, Clone)]
    struct FieldElement([u64; 5]);

    impl FieldElement {
        const ONE: Self = Self([1, 0, 0, 0, 0]);
        const EDWARDS_D: Self = Self([
            929955233495203,
            466365720129213,
            1662059464998953,
            2033849074728123,
            1442794654840575,
        ]);

        #[rustfmt::skip] // keep alignment of bit shifts
        const fn from_bytes(bytes: &[u8; 32]) -> Self {
            const fn load8(input: &[u8], i: usize) -> u64 {
                let input = input.split_at(i).1;
                (input[0] as u64)
                | ((input[1] as u64) << 8)
                | ((input[2] as u64) << 16)
                | ((input[3] as u64) << 24)
                | ((input[4] as u64) << 32)
                | ((input[5] as u64) << 40)
                | ((input[6] as u64) << 48)
                | ((input[7] as u64) << 56)
            }

            let low_51_bit_mask = (1u64 << 51) - 1;
            Self([
            // load bits [  0, 64), no shift
               load8(bytes, 0)        & low_51_bit_mask,
            // load bits [ 48,112), shift to [ 51,112)
              (load8(bytes, 6) >>  3) & low_51_bit_mask,
            // load bits [ 96,160), shift to [102,160)
              (load8(bytes, 12) >>  6) & low_51_bit_mask,
            // load bits [152,216), shift to [153,216)
              (load8(bytes, 19) >>  1) & low_51_bit_mask,
            // load bits [192,256), shift to [204,112)
              (load8(bytes, 24) >> 12) & low_51_bit_mask,
            ])
        }
        const fn square(&self) -> Self {
            self.pow2k(1)
        }
        #[rustfmt::skip] // keep alignment of c* calculations
        const fn pow2k(&self, mut k: u32) -> Self {
            debug_assert!( k > 0 );

            /// Multiply two 64-bit integers with 128 bits of output.
            #[inline(always)]
            const fn m(x: u64, y: u64) -> u128 {
                (x as u128) * (y as u128)
            }

            let mut a: [u64; 5] = self.0;

            loop {
                // Precondition: assume input limbs a[i] are bounded as
                //
                // a[i] < 2^(51 + b)
                //
                // where b is a real parameter measuring the "bit excess" of the limbs.

                // Precomputation: 64-bit multiply by 19.
                //
                // This fits into a u64 whenever 51 + b + lg(19) < 64.
                //
                // Since 51 + b + lg(19) < 51 + 4.25 + b
                //                       = 55.25 + b,
                // this fits if b < 8.75.
                let a3_19 = 19 * a[3];
                let a4_19 = 19 * a[4];

                // Multiply to get 128-bit coefficients of output.
                //
                // The 128-bit multiplications by 2 turn into 1 slr + 1 slrd each,
                // which doesn't seem any better or worse than doing them as precomputations
                // on the 64-bit inputs.
                let     c0: u128 = m(a[0],  a[0]) + 2*( m(a[1], a4_19) + m(a[2], a3_19) );
                let mut c1: u128 = m(a[3], a3_19) + 2*( m(a[0],  a[1]) + m(a[2], a4_19) );
                let mut c2: u128 = m(a[1],  a[1]) + 2*( m(a[0],  a[2]) + m(a[4], a3_19) );
                let mut c3: u128 = m(a[4], a4_19) + 2*( m(a[0],  a[3]) + m(a[1],  a[2]) );
                let mut c4: u128 = m(a[2],  a[2]) + 2*( m(a[0],  a[4]) + m(a[1],  a[3]) );

                // Same bound as in multiply:
                //    c[i] < 2^(102 + 2*b) * (1+i + (4-i)*19)
                //         < 2^(102 + lg(1 + 4*19) + 2*b)
                //         < 2^(108.27 + 2*b)
                //
                // The carry (c[i] >> 51) fits into a u64 when
                //    108.27 + 2*b - 51 < 64
                //    2*b < 6.73
                //    b < 3.365.
                //
                // So we require b < 3 to ensure this fits.
                debug_assert!(a[0] < (1 << 54));
                debug_assert!(a[1] < (1 << 54));
                debug_assert!(a[2] < (1 << 54));
                debug_assert!(a[3] < (1 << 54));
                debug_assert!(a[4] < (1 << 54));

                const LOW_51_BIT_MASK: u64 = (1u64 << 51) - 1;

                // Casting to u64 and back tells the compiler that the carry is bounded by 2^64, so
                // that the addition is a u128 + u64 rather than u128 + u128.
                c1 += ((c0 >> 51) as u64) as u128;
                a[0] = (c0 as u64) & LOW_51_BIT_MASK;

                c2 += ((c1 >> 51) as u64) as u128;
                a[1] = (c1 as u64) & LOW_51_BIT_MASK;

                c3 += ((c2 >> 51) as u64) as u128;
                a[2] = (c2 as u64) & LOW_51_BIT_MASK;

                c4 += ((c3 >> 51) as u64) as u128;
                a[3] = (c3 as u64) & LOW_51_BIT_MASK;

                let carry: u64 = (c4 >> 51) as u64;
                a[4] = (c4 as u64) & LOW_51_BIT_MASK;

                // To see that this does not overflow, we need a[0] + carry * 19 < 2^64.
                //
                // c4 < a2^2 + 2*a0*a4 + 2*a1*a3 + (carry from c3)
                //    < 2^(102 + 2*b + lg(5)) + 2^64.
                //
                // When b < 3 we get
                //
                // c4 < 2^110.33  so that carry < 2^59.33
                //
                // so that
                //
                // a[0] + carry * 19 < 2^51 + 19 * 2^59.33 < 2^63.58
                //
                // and there is no overflow.
                a[0] += carry * 19;

                // Now a[1] < 2^51 + 2^(64 -51) = 2^51 + 2^13 < 2^(51 + epsilon).
                a[1] += a[0] >> 51;
                a[0] &= LOW_51_BIT_MASK;

                // Now all a[i] < 2^(51 + epsilon) and a = self^(2^k).

                k -= 1;
                if k == 0 {
                    break;
                }
            }

            Self(a)
        }
        const fn add_assign(&mut self, rhs: &Self) {
            let mut i = 0;
            while i < self.0.len() {
                self.0[i] += rhs.0[i];
                i += 1;
            }
        }
        const fn add(&self, rhs: &Self) -> Self {
            let mut output = *self;
            output.add_assign(rhs);
            output
        }
        const fn sub(&self, rhs: &Self) -> Self {
            // To avoid underflow, first add a multiple of p.
            // Choose 16*p = p << 4 to be larger than 54-bit _rhs.
            //
            // If we could statically track the bitlengths of the limbs
            // of every FieldElement51, we could choose a multiple of p
            // just bigger than _rhs and avoid having to do a reduction.
            //
            // Since we don't yet have type-level integers to do this, we
            // have to add an explicit reduction call here.
            Self::reduce([
                (self.0[0] + 36028797018963664u64) - rhs.0[0],
                (self.0[1] + 36028797018963952u64) - rhs.0[1],
                (self.0[2] + 36028797018963952u64) - rhs.0[2],
                (self.0[3] + 36028797018963952u64) - rhs.0[3],
                (self.0[4] + 36028797018963952u64) - rhs.0[4],
            ])
        }
        #[rustfmt::skip] // keep alignment of c* calculations
        const fn mul(&self, rhs: &Self) -> Self {
            /// Helper function to multiply two 64-bit integers with 128
            /// bits of output.
            #[inline(always)]
            const fn m(x: u64, y: u64) -> u128 { (x as u128) * (y as u128) }

            // Alias self, _rhs for more readable formulas
            let a: &[u64; 5] = &self.0;
            let b: &[u64; 5] = &rhs.0;

            // Precondition: assume input limbs a[i], b[i] are bounded as
            //
            // a[i], b[i] < 2^(51 + b)
            //
            // where b is a real parameter measuring the "bit excess" of the limbs.

            // 64-bit precomputations to avoid 128-bit multiplications.
            //
            // This fits into a u64 whenever 51 + b + lg(19) < 64.
            //
            // Since 51 + b + lg(19) < 51 + 4.25 + b
            //                       = 55.25 + b,
            // this fits if b < 8.75.
            let b1_19 = b[1] * 19;
            let b2_19 = b[2] * 19;
            let b3_19 = b[3] * 19;
            let b4_19 = b[4] * 19;

            // Multiply to get 128-bit coefficients of output
            let     c0: u128 = m(a[0], b[0]) + m(a[4], b1_19) + m(a[3], b2_19) + m(a[2], b3_19) + m(a[1], b4_19);
            let mut c1: u128 = m(a[1], b[0]) + m(a[0],  b[1]) + m(a[4], b2_19) + m(a[3], b3_19) + m(a[2], b4_19);
            let mut c2: u128 = m(a[2], b[0]) + m(a[1],  b[1]) + m(a[0],  b[2]) + m(a[4], b3_19) + m(a[3], b4_19);
            let mut c3: u128 = m(a[3], b[0]) + m(a[2],  b[1]) + m(a[1],  b[2]) + m(a[0],  b[3]) + m(a[4], b4_19);
            let mut c4: u128 = m(a[4], b[0]) + m(a[3],  b[1]) + m(a[2],  b[2]) + m(a[1],  b[3]) + m(a[0] , b[4]);

            // How big are the c[i]? We have
            //
            //    c[i] < 2^(102 + 2*b) * (1+i + (4-i)*19)
            //         < 2^(102 + lg(1 + 4*19) + 2*b)
            //         < 2^(108.27 + 2*b)
            //
            // The carry (c[i] >> 51) fits into a u64 when
            //    108.27 + 2*b - 51 < 64
            //    2*b < 6.73
            //    b < 3.365.
            //
            // So we require b < 3 to ensure this fits.
            debug_assert!(a[0] < (1 << 54)); debug_assert!(b[0] < (1 << 54));
            debug_assert!(a[1] < (1 << 54)); debug_assert!(b[1] < (1 << 54));
            debug_assert!(a[2] < (1 << 54)); debug_assert!(b[2] < (1 << 54));
            debug_assert!(a[3] < (1 << 54)); debug_assert!(b[3] < (1 << 54));
            debug_assert!(a[4] < (1 << 54)); debug_assert!(b[4] < (1 << 54));

            // Casting to u64 and back tells the compiler that the carry is
            // bounded by 2^64, so that the addition is a u128 + u64 rather
            // than u128 + u128.

            const LOW_51_BIT_MASK: u64 = (1u64 << 51) - 1;
            let mut out = [0u64; 5];

            c1 += ((c0 >> 51) as u64) as u128;
            out[0] = (c0 as u64) & LOW_51_BIT_MASK;

            c2 += ((c1 >> 51) as u64) as u128;
            out[1] = (c1 as u64) & LOW_51_BIT_MASK;

            c3 += ((c2 >> 51) as u64) as u128;
            out[2] = (c2 as u64) & LOW_51_BIT_MASK;

            c4 += ((c3 >> 51) as u64) as u128;
            out[3] = (c3 as u64) & LOW_51_BIT_MASK;

            let carry: u64 = (c4 >> 51) as u64;
            out[4] = (c4 as u64) & LOW_51_BIT_MASK;

            // To see that this does not overflow, we need out[0] + carry * 19 < 2^64.
            //
            // c4 < a0*b4 + a1*b3 + a2*b2 + a3*b1 + a4*b0 + (carry from c3)
            //    < 5*(2^(51 + b) * 2^(51 + b)) + (carry from c3)
            //    < 2^(102 + 2*b + lg(5)) + 2^64.
            //
            // When b < 3 we get
            //
            // c4 < 2^110.33  so that carry < 2^59.33
            //
            // so that
            //
            // out[0] + carry * 19 < 2^51 + 19 * 2^59.33 < 2^63.58
            //
            // and there is no overflow.
            out[0] += carry * 19;

            // Now out[1] < 2^51 + 2^(64 -51) = 2^51 + 2^13 < 2^(51 + epsilon).
            out[1] += out[0] >> 51;
            out[0] &= LOW_51_BIT_MASK;

            // Now out[i] < 2^(51 + epsilon) for all i.
            Self(out)
        }
        const fn neg(&self) -> Self {
            Self::reduce([
                36028797018963664u64 - self.0[0],
                36028797018963952u64 - self.0[1],
                36028797018963952u64 - self.0[2],
                36028797018963952u64 - self.0[3],
                36028797018963952u64 - self.0[4],
            ])
        }

        const fn eq(&self, other: &Self) -> bool {
            let mut i = 0;
            while i < self.0.len() {
                if self.0[i] != other.0[i] {
                    return false;
                }
                i += 1;
            }
            true
        }

        #[inline(always)]
        const fn reduce(mut limbs: [u64; 5]) -> Self {
            const LOW_51_BIT_MASK: u64 = (1u64 << 51) - 1;

            // Since the input limbs are bounded by 2^64, the biggest
            // carry-out is bounded by 2^13.
            //
            // The biggest carry-in is c4 * 19, resulting in
            //
            // 2^51 + 19*2^13 < 2^51.0000000001
            //
            // Because we don't need to canonicalize, only to reduce the
            // limb sizes, it's OK to do a "weak reduction", where we
            // compute the carry-outs in parallel.

            let c0 = limbs[0] >> 51;
            let c1 = limbs[1] >> 51;
            let c2 = limbs[2] >> 51;
            let c3 = limbs[3] >> 51;
            let c4 = limbs[4] >> 51;

            limbs[0] &= LOW_51_BIT_MASK;
            limbs[1] &= LOW_51_BIT_MASK;
            limbs[2] &= LOW_51_BIT_MASK;
            limbs[3] &= LOW_51_BIT_MASK;
            limbs[4] &= LOW_51_BIT_MASK;

            limbs[0] += c4 * 19;
            limbs[1] += c0;
            limbs[2] += c1;
            limbs[3] += c2;
            limbs[4] += c3;

            Self(limbs)
        }

        #[rustfmt::skip] // keep alignment of explanatory comments
        const fn pow22501(&self) -> (Self, Self) {
            // Instead of managing which temporary variables are used
            // for what, we define as many as we need and leave stack
            // allocation to the compiler
            //
            // Each temporary variable t_i is of the form (self)^e_i.
            // Squaring t_i corresponds to multiplying e_i by 2,
            // so the pow2k function shifts e_i left by k places.
            // Multiplying t_i and t_j corresponds to adding e_i + e_j.
            //
            // Temporary t_i                      Nonzero bits of e_i
            //
            let t0  = self.square();           // 1         e_0 = 2^1
            let t1  = t0.square().square();    // 3         e_1 = 2^3
            let t2  = self.mul(&t1);           // 3,0       e_2 = 2^3 + 2^0
            let t3  = t0.mul(&t2);             // 3,1,0
            let t4  = t3.square();             // 4,2,1
            let t5  = t2.mul(&t4);             // 4,3,2,1,0
            let t6  = t5.pow2k(5);             // 9,8,7,6,5
            let t7  = t6.mul(&t5);             // 9,8,7,6,5,4,3,2,1,0
            let t8  = t7.pow2k(10);            // 19..10
            let t9  = t8.mul(&t7);             // 19..0
            let t10 = t9.pow2k(20);            // 39..20
            let t11 = t10.mul(&t9);            // 39..0
            let t12 = t11.pow2k(10);           // 49..10
            let t13 = t12.mul(&t7);            // 49..0
            let t14 = t13.pow2k(50);           // 99..50
            let t15 = t14.mul(&t13);           // 99..0
            let t16 = t15.pow2k(100);          // 199..100
            let t17 = t16.mul(&t15);           // 199..0
            let t18 = t17.pow2k(50);           // 249..50
            let t19 = t18.mul(&t13);           // 249..0

            (t19, t3)
        }

        /// Raise this field element to the power (p-5)/8 = 2^252 -3.
        #[rustfmt::skip] // keep alignment of explanatory comments
        #[allow(clippy::let_and_return)]
        const fn pow_p58(&self) -> Self {
            // The bits of (p-5)/8 are 101111.....11.
            //
            //                                 nonzero bits of exponent
            let (t19, _) = self.pow22501();    // 249..0
            let t20 = t19.pow2k(2);            // 251..2
            let t21 = self.mul(&t20);          // 251..2,0

            t21
        }

        /// Given `FieldElements` `u` and `v`, compute if either `sqrt(u/v)`
        /// or `sqrt(i*u/v)` is possible.
        ///
        /// # Return
        ///
        /// - `true` if `v` is nonzero and `u/v` is square;
        /// - `true` if `u` is zero;
        /// - `false` if `v` is zero and `u` is nonzero;
        /// - `false` if `u/v` is nonsquare (so `i*u/v` is square).
        ///
        const fn can_sqrt_ratio_i(u: &FieldElement, v: &FieldElement) -> bool {
            // Using the same trick as in ed25519 decoding, we merge the
            // inversion, the square root, and the square test as follows.
            //
            // To compute sqrt(α), we can compute β = α^((p+3)/8).
            // Then β^2 = ±α, so multiplying β by sqrt(-1) if necessary
            // gives sqrt(α).
            //
            // To compute 1/sqrt(α), we observe that
            //    1/β = α^(p-1 - (p+3)/8) = α^((7p-11)/8)
            //                            = α^3 * (α^7)^((p-5)/8).
            //
            // We can therefore compute sqrt(u/v) = sqrt(u)/sqrt(v)
            // by first computing
            //    r = u^((p+3)/8) v^(p-1-(p+3)/8)
            //      = u u^((p-5)/8) v^3 (v^7)^((p-5)/8)
            //      = (uv^3) (uv^7)^((p-5)/8).
            //
            // If v is nonzero and u/v is square, then r^2 = ±u/v,
            //                                     so vr^2 = ±u.
            // If vr^2 =  u, then sqrt(u/v) = r.
            // If vr^2 = -u, then sqrt(u/v) = r*sqrt(-1).
            //
            // If v is zero, r is also zero.

            let v3 = v.square().mul(v);
            let v7 = v3.square().mul(v);
            let r = u.mul(&v3).mul(&u.mul(&v7)).pow_p58();
            let check = v.mul(&r.square());

            check.eq(&u) || check.eq(&u.neg())
        }
    }

    let y = FieldElement::from_bytes(&bytes);
    let z = FieldElement::ONE;
    let yy = y.square();
    let u = yy.sub(&z); // u =  y²-1
    let v = yy.mul(&FieldElement::EDWARDS_D).add(&z); // v = dy²+1
    FieldElement::can_sqrt_ratio_i(&u, &v)
}

impl AsRef<[u8]> for Pubkey {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl AsMut<[u8]> for Pubkey {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0[..]
    }
}
impl From<[u8; 32]> for Pubkey {
    #[inline]
    fn from(from: [u8; 32]) -> Self {
        Self(from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_program_address() {
        let program_id = Pubkey::from_str_const("BPFLoaderUpgradeab1e11111111111111111111111");
        let public_key = Pubkey::from_str_const("SeedPubey1111111111111111111111111111111111");
        assert_eq!(
            Pubkey::create_program_address(&[b"", &[1]], &program_id),
            Ok(Pubkey::from_str_const(
                "BwqrghZA2htAcqq8dzP1WDAhTXYTYWj7CHxF5j7TDBAe"
            )),
        );
        assert_eq!(
            Pubkey::create_program_address(&[b"Talking", b"Squirrels"], &program_id),
            Ok(Pubkey::from_str_const(
                "2fnQrngrQT4SeLcdToJAD96phoEjNL2man2kfRLCASVk"
            )),
        );

        assert_eq!(
            Pubkey::create_program_address(&[public_key.as_ref(), &[1]], &program_id),
            Ok(Pubkey::from_str_const(
                "976ymqVnfE32QFe6NfGDctSvVa36LWnvYxhU6G2232YL"
            )),
        );

        assert_ne!(
            Pubkey::create_program_address(&[b"Talking", b"Squirrels"], &program_id).unwrap(),
            Pubkey::create_program_address(&[b"Talking"], &program_id).unwrap(),
        );
    }
}
