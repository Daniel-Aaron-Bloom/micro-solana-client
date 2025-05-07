use micro_solana_client::{parse_idl, pubkey, Pubkey};

pub static ID: Pubkey = pubkey!("My11111111111111111111111111111111111111111");

parse_idl! {"./idls/matching_engine.json"}
parse_idl! {"./idls/token_router.json"}
parse_idl! {"./idls/message_transmitter.json"}
pub mod old {
    micro_solana_client::parse_idl! {"./idls/old.json"}
    pub use idl::*;
}

// mod new {
//     micro_solana_client::parse_idl! {"./idls/new.json"}
//     pub use idl::*;
// }

const _: () = {
    let _ = message_transmitter::errors::PROGRAM_PAUSED;
    let v = micro_solana_client::eager2::eager! {
        micro_solana_client::__is_ident!(unstringify!("foo::bar"))
    };
    assert!(!v);
};
