use alloc::vec::Vec;

use crate::Pubkey;

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AccountMeta {
    /// An account's public key.
    pub pubkey: Pubkey,

    /// True if an `Instruction` requires a `Transaction` signature matching `pubkey`.
    pub is_signer: bool,

    /// True if the account data or metadata may be mutated during program execution.
    pub is_writable: bool,
}

#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instruction {
    /// Pubkey of the program that executes this instruction.
    pub program_id: Pubkey,

    /// Metadata describing accounts that should be passed to the program.
    pub accounts: Vec<AccountMeta>,

    /// Opaque data passed to the program for its own interpretation.
    pub data: Vec<u8>,
}

impl AccountMeta {
    /// Construct metadata for a writable account.
    pub fn new(pubkey: Pubkey, is_signer: bool) -> Self {
        Self {
            pubkey,
            is_signer,
            is_writable: true,
        }
    }

    /// Construct metadata for a read-only account.
    pub fn new_readonly(pubkey: Pubkey, is_signer: bool) -> Self {
        Self {
            pubkey,
            is_signer,
            is_writable: false,
        }
    }
}
