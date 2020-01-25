use hopr_polkadot_runtime::{
	AccountId, BalancesConfig, ConsensusConfig, GenesisConfig, IndicesConfig, SudoConfig,
	TimestampConfig,
};
use primitives::{ed25519, sr25519, Pair};
use substrate_service;

use ed25519::Public as AuthorityId;

// Note this is the URL for the telemetry server
//const STAGING_TELEMETRY_URL: &str = "wss://telemetry.polkadot.io/submit/";

/// Specialized `ChainSpec`. This is a specialization of the general Substrate ChainSpec type.
pub type ChainSpec = substrate_service::ChainSpec<GenesisConfig>;

/// The chain specification option. This is expected to come in from the CLI and
/// is little more than one of a number of alternatives which can easily be converted
/// from a string (`--chain=...`) into a `ChainSpec`.
#[derive(Clone, Debug)]
pub enum Alternative {
	/// Whatever the current runtime is, with just Alice as an auth.
	Development,
	/// Whatever the current runtime is, with simple Alice/Bob auths.
	LocalTestnet,
}

fn authority_key(s: &str) -> AuthorityId {
	ed25519::Pair::from_string(&format!("//{}", s), None)
		.expect("static values are valid; qed")
		.public()
}

fn _account_key(s: &str) -> AccountId {
	sr25519::Pair::from_string(&format!("//{}", s), None)
		.expect("static values are valid; qed")
		.public()
}

fn my_account_key(pub_key: [u8; 32]) -> AccountId {
	sr25519::Public::from_raw(pub_key)
}

impl Alternative {
	/// Get an actual chain config from one of the alternatives.
	pub(crate) fn load(self) -> Result<ChainSpec, String> {
		let _alice: [u8; 32] = [
			28, 39, 75, 59, 54, 152, 95, 137, 242, 131, 47, 107, 200, 142, 143, 53, 115, 196, 107,
			230, 62, 105, 197, 20, 5, 131, 44, 141, 168, 79, 38, 98,
		];

		let _bob: [u8; 32] = [
			156, 9, 185, 27, 96, 69, 104, 176, 178, 114, 0, 109, 168, 186, 15, 65, 96, 215, 95,
			194, 175, 85, 60, 88, 115, 130, 136, 97, 50, 230, 126, 47,
		];

		let _charlie: [u8; 32] = [
			142, 76, 118, 231, 140, 213, 254, 237, 158, 56, 120, 250, 139, 144, 77, 140, 78, 213,
			116, 246, 187, 181, 125, 185, 199, 31, 19, 14, 59, 138, 234, 72,
		];

		let _dave: [u8; 32] = [
			186, 23, 125, 21, 149, 208, 26, 240, 200, 43, 12, 136, 146, 223, 143, 201, 19, 120, 77,
			155, 176, 77, 55, 221, 38, 139, 180, 186, 183, 175, 247, 82,
		];

		let _ed: [u8; 32] = [
			208, 157, 79, 29, 167, 195, 6, 139, 20, 106, 61, 247, 143, 187, 250, 143, 166, 173, 90,
			32, 128, 107, 121, 63, 137, 94, 17, 191, 174, 56, 73, 112,
		];

		let _fred: [u8; 32] = [
			98, 116, 134, 130, 44, 204, 25, 213, 155, 197, 63, 101, 166, 13, 64, 130, 164, 155, 1,
			63, 249, 192, 125, 102, 197, 108, 140, 188, 182, 183, 224, 54,
		];

		let _george: [u8; 32] = [
			148, 54, 200, 195, 14, 10, 87, 98, 227, 7, 175, 202, 26, 250, 138, 194, 9, 30, 202,
			248, 16, 31, 35, 137, 180, 15, 43, 27, 7, 74, 99, 99,
		];

		Ok(match self {
			Alternative::Development => ChainSpec::from_genesis(
				"Development",
				"dev",
				|| {
					testnet_genesis(
						vec![],
						vec![
							// Alice
							my_account_key([
								28, 39, 75, 59, 54, 152, 95, 137, 242, 131, 47, 107, 200, 142, 143,
								53, 115, 196, 107, 230, 62, 105, 197, 20, 5, 131, 44, 141, 168, 79,
								38, 98,
							]),
							// Bob
							my_account_key([
								156, 9, 185, 27, 96, 69, 104, 176, 178, 114, 0, 109, 168, 186, 15,
								65, 96, 215, 95, 194, 175, 85, 60, 88, 115, 130, 136, 97, 50, 230,
								126, 47,
							]),
							// Charlie
							my_account_key([
								142, 76, 118, 231, 140, 213, 254, 237, 158, 56, 120, 250, 139, 144,
								77, 140, 78, 213, 116, 246, 187, 181, 125, 185, 199, 31, 19, 14,
								59, 138, 234, 72,
							]),
							// Dave
							my_account_key([
								186, 23, 125, 21, 149, 208, 26, 240, 200, 43, 12, 136, 146, 223,
								143, 201, 19, 120, 77, 155, 176, 77, 55, 221, 38, 139, 180, 186,
								183, 175, 247, 82,
							]),
							// Ed
							my_account_key([
								208, 157, 79, 29, 167, 195, 6, 139, 20, 106, 61, 247, 143, 187,
								250, 143, 166, 173, 90, 32, 128, 107, 121, 63, 137, 94, 17, 191,
								174, 56, 73, 112,
							]),
							// Fred
							my_account_key([
								98, 116, 134, 130, 44, 204, 25, 213, 155, 197, 63, 101, 166, 13,
								64, 130, 164, 155, 1, 63, 249, 192, 125, 102, 197, 108, 140, 188,
								182, 183, 224, 54,
							]),
							// George
							my_account_key([
								148, 54, 200, 195, 14, 10, 87, 98, 227, 7, 175, 202, 26, 250, 138,
								194, 9, 30, 202, 248, 16, 31, 35, 137, 180, 15, 43, 27, 7, 74, 99,
								99,
							]),
						],
						// Alice
						my_account_key([
							28, 39, 75, 59, 54, 152, 95, 137, 242, 131, 47, 107, 200, 142, 143, 53,
							115, 196, 107, 230, 62, 105, 197, 20, 5, 131, 44, 141, 168, 79, 38, 98,
						]),
					)
				},
				vec![],
				None,
				None,
				None,
				None,
			),
			Alternative::LocalTestnet => ChainSpec::from_genesis(
				"Local Testnet",
				"local_testnet",
				|| {
					testnet_genesis(
						vec![authority_key("Alice"), authority_key("Bob")],
						vec![
							// Alice
							my_account_key([
								28, 39, 75, 59, 54, 152, 95, 137, 242, 131, 47, 107, 200, 142, 143,
								53, 115, 196, 107, 230, 62, 105, 197, 20, 5, 131, 44, 141, 168, 79,
								38, 98,
							]),
							// Bob
							my_account_key([
								156, 9, 185, 27, 96, 69, 104, 176, 178, 114, 0, 109, 168, 186, 15,
								65, 96, 215, 95, 194, 175, 85, 60, 88, 115, 130, 136, 97, 50, 230,
								126, 47,
							]),
							// Charlie
							my_account_key([
								142, 76, 118, 231, 140, 213, 254, 237, 158, 56, 120, 250, 139, 144,
								77, 140, 78, 213, 116, 246, 187, 181, 125, 185, 199, 31, 19, 14,
								59, 138, 234, 72,
							]),
							// Dave
							my_account_key([
								186, 23, 125, 21, 149, 208, 26, 240, 200, 43, 12, 136, 146, 223,
								143, 201, 19, 120, 77, 155, 176, 77, 55, 221, 38, 139, 180, 186,
								183, 175, 247, 82,
							]),
							// Ed
							my_account_key([
								208, 157, 79, 29, 167, 195, 6, 139, 20, 106, 61, 247, 143, 187,
								250, 143, 166, 173, 90, 32, 128, 107, 121, 63, 137, 94, 17, 191,
								174, 56, 73, 112,
							]),
							// Fred
							my_account_key([
								98, 116, 134, 130, 44, 204, 25, 213, 155, 197, 63, 101, 166, 13,
								64, 130, 164, 155, 1, 63, 249, 192, 125, 102, 197, 108, 140, 188,
								182, 183, 224, 54,
							]),
							// George
							my_account_key([
								148, 54, 200, 195, 14, 10, 87, 98, 227, 7, 175, 202, 26, 250, 138,
								194, 9, 30, 202, 248, 16, 31, 35, 137, 180, 15, 43, 27, 7, 74, 99,
								99,
							]),
						],
						// Alice
						my_account_key([
							28, 39, 75, 59, 54, 152, 95, 137, 242, 131, 47, 107, 200, 142, 143, 53,
							115, 196, 107, 230, 62, 105, 197, 20, 5, 131, 44, 141, 168, 79, 38, 98,
						]),
					)
				},
				vec![],
				None,
				None,
				None,
				None,
			),
		})
	}

	pub(crate) fn from(s: &str) -> Option<Self> {
		match s {
			"dev" => Some(Alternative::Development),
			"" | "local" => Some(Alternative::LocalTestnet),
			_ => None,
		}
	}
}

fn testnet_genesis(
	initial_authorities: Vec<AuthorityId>,
	endowed_accounts: Vec<AccountId>,
	root_key: AccountId,
) -> GenesisConfig {
	GenesisConfig {
		consensus: Some(ConsensusConfig {
			code: include_bytes!("../runtime/wasm/target/wasm32-unknown-unknown/release/hopr_polkadot_runtime_wasm.compact.wasm").to_vec(),
			authorities: initial_authorities.clone(),
		}),
		system: None,
		timestamp: Some(TimestampConfig {
			minimum_period: 5, // 10 second block time.
		}),
		indices: Some(IndicesConfig {
			ids: endowed_accounts.clone(),
		}),
		balances: Some(BalancesConfig {
			transaction_base_fee: 1,
			transaction_byte_fee: 0,
			existential_deposit: 500,
			transfer_fee: 0,
			creation_fee: 0,
			balances: endowed_accounts.iter().cloned().map(|k|(k, 1 << 60)).collect(),
			vesting: vec![],
		}),
		sudo: Some(SudoConfig {
			key: root_key,
		}),
	}
}
