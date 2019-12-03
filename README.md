# hopr-polkadot

A SRML module whose purpose is to process payments of **[HOPR.network](https://hopr.network)** as a parachain of Polkadot.

For more details about **[HOPR.network](https://hopr.network)** and its integration into the Polkadot ecosystem, see a more detailed description [here](https://github.com/validitylabs/HOPR-PL-Substrate/blob/master/README.md).

# Building

Install Rust:

```bash
curl https://sh.rustup.rs -sSf | sh
```

Install required tools:

```bash
./scripts/init.sh
```

Build the WebAssembly binary:

```bash
./scripts/build.sh
```

Build all native code:

```bash
cargo build
# get some coffee ...
```

# Testing

You can run the unit tests with:

```bash
cargo test -p hopr-polkadot-runtime
```

# Run a parachain node

You can start a development chain with:

```bash
cargo run -- --dev
```

# Run Hopr

```bash
# node hopr --network=polkadot
```
