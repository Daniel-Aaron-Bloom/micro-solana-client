//! A minimal library with for interacting with Solana programs using Anchor IDL files.
//! 
//! The primary innovation of this crate is the ability to parse an individual idl file with a
//! single macro call.
#![no_std]

use eager2::eager_macro;

#[doc(hidden)]
pub extern crate alloc;

#[doc(hidden)]
pub extern crate borsh;

#[doc(hidden)]
pub extern crate lhash;

#[doc(hidden)]
pub extern crate eager2;

#[cfg(not(feature = "solana-pubkey"))]
mod solana_pubkey;
#[cfg(not(feature = "solana-instruction"))]
mod solana_instruction;

pub use solana_pubkey::{Pubkey, PubkeyError, pubkey};
pub use solana_instruction::{AccountMeta, Instruction};

/// Parses an idl file.
/// 
/// The path must be a file path relative to `CARGO_MANIFEST_DIR`.
/// 
/// Creates a `mod` with the name from the idl file containing all the `instructions`, `accounts`,
/// and `types`. Does not currently parse external types in new idl format.
/// 
/// This macro does not do a whole lot of error-checking while parsing, so valid IDL files are
/// expected.
#[macro_export]
#[eager_macro]
macro_rules! parse_idl {
    ($path:literal) => {$crate::eager2::eager! {
        $crate::__parse_idl!(include!(concat!(env!("CARGO_MANIFEST_DIR"), "/", $path)))
    }};
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __ensure_array_of_literals {
    ([$($l:literal),*]) => {};
    ([$($l:literal),* $b:tt $(t:tt)*]) => {
        compile_error!(concat!("non-literal: ", stringify!($b)))
    };
    ($b:tt) => {
        compile_error!(concat!("non-array: ", stringify!($b)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl {
    ({$($name:literal : $content:tt),*}) => {$crate::eager2::lazy!{
        $crate::__expand_idl!{eager!{
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"address"})]{
                address: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"metadata"})]{
                metadata: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"docs"})]{
                docs: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"instructions"})]{
                instructions: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"accounts"})]{
                accounts: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"events"})]{
                events: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"errors"})]{
                errors: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"types"})]{
                types: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"constants"})]{
                constants: $content,
            }{})*
        }}
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl {
    (
        address: $address:literal,
        metadata: { $($md_name:literal: $md_val:tt),* },
        $(docs: $docs:tt,)?
        instructions: [ $($instruction:tt),* ],
        $(accounts: [ $($account:tt),* ],)?
        $(events: [ $($event:tt),* ],)?
        $(errors: [ $($error:tt),* ],)?
        $(types: [ $($type:tt),* ],)?
        $(constants: [ $($constant:tt),* ],)?
    ) => {$crate::eager2::lazy!{
        $crate::__idl_make_mod!{eager!{
            #[allow(unused_imports)]
            #[allow(dead_code)]
            __metadata {
                vis: pub,
                $(eager_if![token_eq!({$md_name}, {"name"})]{
                    name: $md_val,
                }{})*
                $(eager_if![token_eq!({$md_name}, {"description"})]{
                    docs: [$md_val],
                }{})*
                $(docs: $docs,)?
            }

            mod imports {
                use super::super::*;
            }

            pub const ADDRESS: $crate::Pubkey = $crate::Pubkey::from_str_const($address);
            suspend_eager!{
                pub mod instructions {
                    use super::types as __types;
                    $($crate::__parse_idl_instruction!{new; $instruction})*
                }
                $(pub mod accounts {
                    use super::types as __types;
                    $($crate::__parse_idl_maybe_discriminator!{new; $account})*
                })?
                $(pub mod events {
                    use super::types as __types;
                    $($crate::__parse_idl_maybe_discriminator!{new; $event})*
                })?
                $(pub mod errors {
                    use super::types as __types;
                    $($crate::__parse_idl_error!{$error})*
                })?
                $(pub mod types {
                    use super::types as __types;
                    $($crate::__parse_idl_type!{new; $type})*
                })?
                $(pub mod constants {
                    use super::types as __types;
                    $($crate::__parse_idl_constant!{new; $constant})*
                })?
            }
        }}
    }};

    (
        name: $name:literal,
        $(metadata: { $($md_name:literal: $md_val:tt),* },)?
        $(docs: $docs:tt,)?
        instructions: [ $($instruction:tt),* ],
        $(accounts: [ $($account:tt),* ],)?
        $(events: [ $($event:tt),* ],)?
        $(errors: [ $($error:tt),* ],)?
        $(types: [ $($type:tt),* ],)?
        $(constants: [ $($constant:tt),* ],)?
    ) => {$crate::eager2::lazy!{
        $crate::__idl_make_mod!{eager!{
            #[allow(unused_imports)]
            #[allow(dead_code)]
            __metadata {
                vis: pub,
                name: $name,
                $($(eager_if![token_eq!({$md_name}, {"description"})]{
                    docs: [$md_val],
                }{})*)?
                $(docs: $docs,)?
            }

            pub const ADDRESS: $crate::Pubkey = eager_coalesce!{
                {$($(eager_if![token_eq!({$md_name}, {"address"})]{$crate::Pubkey::from_str_const($md_val)}{})*)?},
                {$crate::Pubkey::new_from_array([0; 32])}
            };
            suspend_eager!{
                pub mod instructions {
                    use super::types as __types;
                    $($crate::__parse_idl_instruction!{old; $instruction})*
                }
                $(pub mod accounts {
                    use super::types as __types;
                    $($crate::__parse_idl_maybe_discriminator!{old; $account})*
                })?
                $(pub mod events {
                    use super::types as __types;
                    $($crate::__parse_idl_maybe_discriminator!{old; $event})*
                })?
                $(pub mod errors {
                    use super::types as __types;
                    $($crate::__parse_idl_error!{$error})*
                })?
                $(pub mod types {
                    use super::types as __types;
                    $($crate::__parse_idl_type!{old; $type})*
                })?
                $(pub mod constants {
                    use super::types as __types;
                    $($crate::__parse_idl_constant!{old; $constant})*
                })?
            }
        }}
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_instruction {
    ($ver:ident; { $($name:literal : $content:tt),* }) => {$crate::eager2::lazy!{
        $crate::__expand_idl_instruction!{$ver; eager!{
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"docs"})]{
                docs: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"discriminator"})]{
                discriminator: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"accounts"})]{
                accounts: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"args"})]{
                args: $content,
            }{})*
        }}
    }};

    ($content:tt) => {
        compile_error!(concat!("idl instruction failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_instruction {
    (
        $ver:ident;
        name: $name:literal,
        $(docs: $docs:tt,)?
        $(discriminator: $discriminator:tt,)?
        accounts: $accounts:tt,
        args: $args:tt,
    ) => {$crate::eager2::lazy!{
        $($crate::__ensure_array_of_literals!{$docs})?
        $($crate::__ensure_array_of_literals!{$discriminator})?

        $crate::__idl_make_mod!{eager!{
            #[allow(unused_imports)]
            #[allow(dead_code)]
            __metadata {
                vis: pub,
                name: $name,
                $(docs: $docs,)?
            }

            impl Type {
                pub const DISCRIMINATOR_NAME: &str = concat!("global:", ccase!($name, t: "snake"));
                pub const DISCRIMINATOR: &[u8] = 
                    eager_if![token_eq!({$($discriminator)?}, {})]
                        {
                            $crate::lhash::sha256(Self::DISCRIMINATOR_NAME.as_bytes())
                                .first_chunk::<8>().unwrap()
                        }
                        {&$($discriminator)?};
            }
            suspend_eager!{$crate::__parse_idl_accounts!{$name, $accounts}}

            const _: () = {
                use super::__types;

                suspend_eager!{$crate::__parse_idl_args!{$ver; $args}}
            };
        }}
    }};

    ($content:tt) => {
        compile_error!(concat!("idl instruction failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_accounts {
    // Loop through each account
    ($struct_name:literal, [ $($content:tt),* ]) => {$crate::eager2::lazy!{
        $crate::__expand_idl_accounts!{
            $struct_name,
            eager!{$({$crate::__parse_idl_account!{$content}})*}
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl accounts failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_account {
    // Turn each account into a module
    ({ $($name:literal : $content:tt),* }) => {$crate::eager2::eager!{
        $(eager_if![token_eq!({$name}, {"name"})]{
            name: $content,
            field_name: unstringify!(ccase!($content, t: "snake")),
        }{})*
        $(eager_if![token_eq!({$name}, {"docs"})]{
            docs: $content,
        }{})*
        $(eager_if![token_eq!({$name}, {"writable"})]{
            writable: $content,
        }{})*
        $(eager_if![token_eq!({$name}, {"isMut"})]{
            writable: $content,
        }{})*
        $(eager_if![token_eq!({$name}, {"signer"})]{
            signer: $content,
        }{})*
        $(eager_if![token_eq!({$name}, {"isSigner"})]{
            signer: $content,
        }{})*
        $(eager_if![token_eq!({$name}, {"accounts"})]{
            accounts: $content,
        }{})*
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl account failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_accounts {
    ($struct_name:literal, $({
        name: $name:literal,
        field_name: $field_name:ident,
        $(docs: [$($docs:literal),*],)?
        $(writable: $writable:literal,)?
        $(signer: $signer:literal,)?
        $(accounts: $accounts:tt,)?
    })*) => {$crate::eager2::lazy!{
        $($crate::__idl_make_mod!{eager!{
            __metadata {
                vis: pub,
                name: $name,
                $(docs: [$($docs),*],)?
            }

            eager_if![token_eq!({$($accounts)?}, {})]{
                pub mod accounts{}

                pub const WRITABLE: bool = eager_coalesce!{{$($writable)?}, {false}};
                pub const SIGNER: bool = eager_coalesce!{{$($signer)?}, {false}};

                pub use $crate::Pubkey as Type;
                pub const SIZE: usize = 32;
                pub fn get_accounts(self_: &Type, account_metas: &mut Vec<$crate::AccountMeta>) {
                    if WRITABLE {
                        account_metas.push($crate::AccountMeta::new(*self_, SIGNER));
                    } else {
                        account_metas.push($crate::AccountMeta::new_readonly(*self_, SIGNER));
                    }
                }
            }{}

            suspend_eager!{$($crate::__parse_idl_accounts!{$name, $accounts})?}
        }})*

        #[derive(Debug, $crate::borsh::BorshSerialize, $crate::borsh::BorshDeserialize, Clone)]
        eager!{
            pub struct ccase!(unstringify!($struct_name), t:"UpperCamel") {$(
                $($(#[doc = $docs])*)?
                pub $field_name: $field_name::Type,
            )*}

            pub use ccase!(unstringify!($struct_name), t:"UpperCamel") as Type;
        }
        
        #[allow(clippy::ptr_arg)]
        pub fn get_accounts(_self: &Type, _account_metas: &mut Vec<$crate::AccountMeta>) {$(
            $field_name::get_accounts(&_self.$field_name, _account_metas);)*
        }

        impl Type {
            pub const NAME: &str = $struct_name;
        }

        impl ::core::fmt::Display for Type {
            fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                ::core::fmt::Debug::fmt(self, f)
            }
        }

        pub mod accounts {$(
            pub use super::$field_name::accounts::*;
        )*}

        pub const SIZE: usize = 0 $(+ $field_name::SIZE)*;
    }};
    ($($content:tt)*) => {
        compile_error!(concat!("idl account failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_args {
    ($ver:ident; [ $( { $($name:literal : $content:tt),* } ),* ]) => {$crate::eager2::lazy!{
        $crate::__expand_idl_args!{$ver; eager!{$({
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: ccase!(unstringify!($content), t:"snake"),
            }{})*
            $(eager_if![token_eq!({$name}, {"type"})]{
                type: $content,
            }{})*
        })*}}
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl args failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_args {
    ($ver:ident; $({
        name: $name:ident,
        type: $type:tt,
    })*) => {$crate::eager2::lazy!{
        impl Type {
            #[allow(clippy::too_many_arguments)]
            pub fn build_instruction(&self, $($name: $crate::__expand_idl_typename!($ver; $type)),*) -> $crate::borsh::io::Result<$crate::Instruction> {
                use $crate::borsh::BorshSerialize;

                let mut accounts = $crate::alloc::vec![];
                let mut data = $crate::alloc::vec::Vec::with_capacity(256);
                data.extend_from_slice(Self::DISCRIMINATOR);
                $($name.serialize(&mut data)?;)*
                get_accounts(self, &mut accounts);
                Ok($crate::Instruction{
                    program_id: super::super::ADDRESS,
                    data,
                    accounts,
                })
            }
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl args failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_maybe_discriminator {
    ($ver:ident; { $($name:literal : $content:tt),* }) => {$crate::eager2::lazy!{
        $crate::__expand_idl_maybe_discriminator!{eager!{$ver;
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"docs"})]{
                docs: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"discriminator"})]{
                discriminator: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"fields"})]{
                fields: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"type"})]{
                type: $content,
            }{})*
        }}
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl maybe discriminator failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_maybe_discriminator {
    (
        $ver:ident;
        name: $name:literal,
        $(docs: [$($docs:literal),*],)?
        discriminator: [ $($discriminator:literal),* ],
    ) => {$crate::eager2::eager!{
        $($(#[doc = $docs])*)?
        pub const ccase!(unstringify!($name), t:"CONSTANT"): &[u8] = &[$($discriminator),*];
    }};

    (
        $ver:ident;
        name: $name:literal,
        $(docs: $docs:tt,)?
        fields: $fields:tt,
    ) => {$crate::__expand_idl_type!{$ver;
        name: $name,
        $(docs: $docs,)?
        type: {
            "kind": "struct",
            "fields": $fields
        },
    }};
    (
        $ver:ident;
        name: $name:literal,
        $(docs: $docs:tt,)?
        type: $type:tt,
    ) => {$crate::__expand_idl_type!{$ver;
        name: $name,
        $(docs: $docs,)?
        type: $type,
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl maybe discriminator failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_error {
    ({ $($name:literal : $content:tt),* }) => {$crate::eager2::eager!{
        $crate::__expand_idl_error!{
            $(eager_if![token_eq!({$name}, {"code"})]{
                code: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"msg"})]{
                msg: $content,
            }{})*
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl accounts failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_error {
    (
        code: $code:literal,
        name: $name:literal,
        $(msg: $msg:literal,)?
    ) => {$crate::eager2::eager!{
        $(#[doc = $msg])?
        pub const ccase!(unstringify!($name), t:"CONSTANT"): u32 = $code;
        
        $(#[doc = $msg])?
        pub struct ccase!(unstringify!($name), t:"UpperCamel");
        impl ccase!(unstringify!($name), t:"UpperCamel") {
            pub const CODE: u32 = $code;
            $(pub const MESSAGE: &str = $msg;)?
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl error failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_type {
    ($ver:ident; { $($name:literal : $content:tt),* }) => {$crate::eager2::lazy!{
        $crate::__expand_idl_type!{eager!{$ver;
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"docs"})]{
                docs: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"type"})]{
                type: $content,
            }{})*

        }}
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl type failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_type {
    (
        new;
        name: $typename:literal,
        $(docs: $docs:tt,)?
        type: { $($name:literal : $content:tt),* },
    ) => {$crate::eager2::eager!{
        eager_if![$crate::__is_ident!{unstringify!{$typename}}]{
            $(eager_if![token_eq!({$name:$content}, {"kind":"struct"})]{
                lazy!{$crate::__parse_idl_type_struct!}
            }{})*
            $(eager_if![token_eq!({$name:$content}, {"kind":"enum"})]{
                lazy!{$crate::__expand_idl_type_enum!}
            }{})*
            {
                new;
                name: $typename,
                $(docs: $docs,)?
                $(eager_if![token_eq!({$name}, {"fields"})]{
                    fields: $content,
                }{})*
                $(eager_if![token_eq!({$name}, {"variants"})]{
                    variants: $content,
                }{})*
            }
        }{
            compile_error!(concat!("type with path not yet supported:\n", $typename))
        }
    }};
    (
        $ver:ident;
        name: $typename:literal,
        $(docs: $docs:tt,)?
        type: { $($name:literal : $content:tt),* },
    ) => {$crate::eager2::eager!{
        $(eager_if![token_eq!({$name:$content}, {"kind":"struct"})]{
            lazy!{$crate::__parse_idl_type_struct!}
        }{})*
        $(eager_if![token_eq!({$name:$content}, {"kind":"enum"})]{
            lazy!{$crate::__expand_idl_type_enum!}
        }{})*
        {
            $ver;
            name: $typename,
            $(docs: $docs,)?
            $(eager_if![token_eq!({$name}, {"fields"})]{
                fields: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"variants"})]{
                variants: $content,
            }{})*
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl type failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_type_struct {
    (
        $ver:ident;
        name: $name:literal,
        $(docs: [$($docs:literal),*],)?
        fields: [ $($field:tt),* ],
    ) => {$crate::eager2::lazy!{
        $crate::__expand_idl_type_struct!{$ver;
            $($(#[doc = $docs])*)?
            #[derive(Debug, Clone, $crate::borsh::BorshSerialize, $crate::borsh::BorshDeserialize)]
            name: $name,
            eager!{$($crate::__parse_idl_type_struct_field!{$field})*}
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl struct type failed to parse:\n", stringify!($($content)*)))
    };
}


#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_type_struct {
    (
        $ver:ident;
        $(#[$struct_meta:meta])*
        name: $name:literal,
    ) => {$crate::eager2::eager!{
        $(#[$struct_meta])*
        pub struct ccase!(unstringify!($name), t:"UpperCamel");
    }};

    (
        $ver:ident;
        $(#[$struct_meta:meta])*
        name: $name:literal,$(
        named_field {
            name: $field_name:literal,
            $(docs: [$($docs:literal),*],)?
            type: $type:tt,
        }
        unnamed_field {})+
    ) => {$crate::eager2::eager!{
        $(#[$struct_meta])*
        pub struct ccase!(unstringify!($name), t:"UpperCamel") {$(
            $($(#[doc = $docs])*)?
            pub ccase!(unstringify!($field_name), t:"snake")
            :
            lazy!($crate::__expand_idl_typename!($ver; $type))
            ,
        )*}

        #[allow(unused)]
        macro_rules! eager!(ccase!(unstringify!($name), t:"snake")) {
            () => {};
        }
        pub(super) use eager!(ccase!(unstringify!($name), t:"snake"));
    }};
    (
        $ver:ident;
        name: $name:literal,$(
            named_field {}
            unnamed_field { $field:tt }
        )+
    ) => {$crate::eager2::eager!{
        pub struct ccase!(unstringify!($name), t:"UpperCamel") ($(
            $($(#[doc = $docs])*)?
            pub lazy!($crate::__expand_idl_typename!($ver; $field)),
        )*)
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl struct type failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_type_enum {
    (
        $ver:ident;
        name: $typename:literal,
        $(docs: [$($docs:literal),*],)?
        variants: [ $($variant:tt),* ],
    ) => {$crate::eager2::eager!{
        $($(#[doc = $docs])*)?
        #[derive(Debug, borsh::BorshSerialize, borsh::BorshDeserialize, Clone)]
        pub enum ccase!(unstringify!($typename), t:"UpperCamel") {
            $($crate::__parse_idl_type_enum_variant!{$ver; $variant})*
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl enum type failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_type_struct_field {
    ({ $($name:literal : $content:tt),* }) => {$crate::eager2::eager!{
        named_field{
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"docs"})]{
                docs: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"type"})]{
                type: $content,
            }{})*
        }
        unnamed_field{
            $(eager_if![token_eq!({$name}, {"defined"})]{
                { "defined" : $content }
            }{})*
            $(eager_if![token_eq!({$name}, {"option"})]{
                { "option" : $content }
            }{})*
            $(eager_if![token_eq!({$name}, {"vec"})]{
                { "vec" : $content }
            }{})*
        }
    }};
    ($name:literal) => {$crate::eager2::eager!{
        named_field{}
        unnamed_field{ $name }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl struct field failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_type_enum_variant {
    ($ver:ident; { $($name:literal : $content:tt),* }) => {$crate::eager2::eager!{
        $crate::__expand_idl_type_enum_variant!{$ver;
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"docs"})]{
                docs: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"fields"})]{
                fields: $content,
            }{})*
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl enum variant failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_type_enum_variant {
    (
        $ver:ident;
        name: $name:literal,
        $(docs: [$($docs:literal),*],)?
    ) => {$crate::eager2::eager!{
        $($(#[doc = $docs])*)?
        ccase!(unstringify!($name), t:"UpperCamel"),
    }};
    (
        $ver:ident;
        name: $name:literal,
        $(docs: [$($docs:literal),*],)?
        fields: [$($field:tt),*],
    ) => {$crate::eager2::eager!{
        $($(#[doc = $docs])*)?
        $crate::__expand_idl_type_enum_variant_fields!{
            $ver;
            name: $name,
            $($crate::__parse_idl_type_struct_field!{$field})*
        }
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl enum variant failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_type_enum_variant_fields {
    (
        $ver:ident;
        name: $name:literal,$(
        named_field {
            name: $field_name:literal,
            $(docs: [$($docs:literal),*],)?
            type: $type:tt,
        }
        unnamed_field {})*
    ) => {$crate::eager2::eager!{
        ccase!(unstringify!($name), t:"UpperCamel"){$(
            $($(#[doc = $docs])*)?
            ccase!(unstringify!($field_name), t:"snake")
            :
            lazy!($crate::__expand_idl_typename!($ver; $type))
            ,
        )*},
    }};
    (
        $ver:ident;
        name: $name:literal,$(
            named_field {}
            unnamed_field { $field:tt }
        )*
    ) => {$crate::eager2::eager!{
        ccase!(unstringify!($name), t:"UpperCamel") ($(
            lazy!($crate::__expand_idl_typename!($ver; $field)),
        )*),
    }};
    ($($content:tt)*) => {
        compile_error!(concat!("idl enum variant failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __expand_idl_typename {
    ($ver:ident, static; "bytes") => {&[u8]};
    ($ver:ident, static; "string") => {&str};
    ($ver:ident, static; "pubkey") => {$crate::Pubkey};
    ($ver:ident, static; "publicKey") => {$crate::Pubkey};
    ($ver:ident, static; $type:literal) => {$crate::eager2::unstringify!($type)};
    ($ver:ident, static; { "array": [ $type:tt, $type_len:literal ] }) => {
        [$crate::__expand_idl_typename!($ver, static; $type); $type_len]
    };
    ($ver:ident, static; { "option": $type:tt}) => {
        Option<$crate::__expand_idl_typename!($ver, static; $type)>
    };

    ($ver:ident, defined; $id:ident) => {$crate::eager2::eager!{
        lazy!{__types::}ccase!($id, t:"UpperCamel")
    }};
    (old, defined; $_:ident $(:: $id:ident)+) => {$crate::__trim_path!{$($id)::+}};
    // (old, defined; $path:path) => {$crate::__trim_path!($path)};
    (new, defined; $path:path) => {$path};

    ($ver:ident; "bytes") => {Vec<u8>};
    ($ver:ident; "string") => {String};
    ($ver:ident; "pubkey") => {$crate::Pubkey};
    ($ver:ident; "publicKey") => {$crate::Pubkey};
    ($ver:ident; $type:literal) => {$crate::eager2::unstringify!($type)};
    ($ver:ident; { "array": [ $type:tt, $type_len:literal ] }) => {
        [$crate::__expand_idl_typename!($ver; $type); $type_len]
    };
    ($ver:ident; { "option": $type:tt}) => {
        Option<$crate::__expand_idl_typename!($ver; $type)>
    };
    ($ver:ident; { "vec": $type:tt}) => {
        Vec<$crate::__expand_idl_typename!($ver; $type)>
    };
    ($ver:ident; { "defined": { "name": $type:literal } }) => {$crate::eager2::lazy!{
        $crate::__expand_idl_typename!($ver, defined; eager!(unstringify!($type)))
    }};
    ($ver:ident; { "defined": $type:literal }) => {$crate::eager2::lazy!{
        $crate::__expand_idl_typename!($ver, defined; eager!(unstringify!($type)))
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl typename failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __parse_idl_constant {
    ($ver:ident; { $($name:literal : $content:tt),* }) => {$crate::eager2::lazy!{
        $crate::__expand_idl_constant!{eager!{$ver;
            $(eager_if![token_eq!({$name}, {"name"})]{
                name: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"docs"})]{
                docs: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"type"})]{
                type: $content,
            }{})*
            $(eager_if![token_eq!({$name}, {"value"})]{
                value: $content,
            }{})*
        }}
    }};

    ($content:tt) => {
        compile_error!(concat!("idl constant failed to parse:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_constant {
    (
        $ver:ident;
        name: $name:literal,
        $(docs: [$($docs:literal),*],)?
        type: $type:tt,
        value: $value:literal,
    ) => {$crate::eager2::eager!{
        $($(#[doc = $docs])*)?
        pub const ccase!(unstringify!($name), t:"CONSTANT"): lazy!{$crate::__expand_idl_typename!{$ver, static; $type}} = $crate::__expand_idl_constant_value!($type; $value);
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl constant failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_constant_value {
    ("string"; $value:literal) => { $value };
    ($type:tt; $value:literal) => {$crate::eager2::eager!{
        $crate::__expand_idl_constant_value_inner!($type; unstringify!($value))
    }};

    ($($content:tt)*) => {
        compile_error!(concat!("idl constant value failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __expand_idl_constant_value_inner {
    ("bytes"; $value:tt) => { &$value };
    ("string"; $value:tt) => { $value };
    ("pubkey"; $value:tt) => { $crate::pubkey!(stringify!($value)) };
    ("publicKey"; $value:tt) => { $crate::pubkey!(stringify!($value)) };
    ($type:literal; $value:tt) => { $value };

    ({ "array": [ $type:tt, $type_len:literal ] }; [$($value:tt),* $(,)?]) => {
        [$($($crate::__expand_idl_constant_value_inner!($type; $value)),*)?]
    };
    ({ "option": $type:tt}; None) => { None };
    ({ "option": $type:tt}; Some($value:tt)) => {
        Some($crate::__expand_idl_constant_value_inner!($type; $value))
    };
    
    ($($content:tt)*) => {
        compile_error!(concat!("idl constant value failed to expand:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __idl_make_mod {
    (
        $(#[$mod_meta:meta])*
        __metadata {
            $(vis: $mod_vis:ident,)?
            name: $mod_name:literal,
            $(docs: [$($docs:literal),*],)*
        }
        $($mod_content:tt)*
    ) => {$crate::eager2::eager!{
        $($(#[doc = $docs])*)*
        $(#[$mod_meta])*
        $($mod_vis)? mod ccase!(unstringify!($mod_name), t:"snake") {
            suspend_eager!{$($mod_content)*}
        }
    }};
    ($($content:tt)*) => {
        compile_error!(concat!("idl failed to make module:\n", stringify!($($content)*)))
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __trim_path {
    ($id:ident) => { $crate::eager2::eager!{
        lazy!{__types::}ccase!($id, t:"UpperCamel")
    }};
    ($_:ident $(:: $id:ident)+) => {$crate::__trim_path!{$($id)::+}};
    // ($content:path) => {
    //     compile_error!(concat!("idl failed to trim path:\n", stringify!($content)))
    // };
    ($_:tt $($content:tt)*) => {
        compile_error!(concat!("idl failed to trim path:\n", stringify!($($content)*)))
    };
    ($($content:tt)*) => {
        compile_error!(concat!("idl failed to trim path:\n", stringify!($($content)*)))
    };
}


#[doc(hidden)]
#[macro_export]
#[eager_macro]
macro_rules! __is_ident {
    ($_:ident) => {true};
    ($($_:tt)*) => {false};
}
