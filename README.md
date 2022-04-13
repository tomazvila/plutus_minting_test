# plutus_minting_test
Example of not working plutus minting emulator trace setup

## Error

```
Slot 00001: *** CONTRACT STOPPED WITH ERROR: "\"WalletContractError (ValidationError (ScriptFailure (EvaluationError [] \\\"BuiltinEvaluationFailure of UnIData\\\")))\""
```
## Setup

```bash
cardano_node@demoserver:~/coreapp_testing/new_repo$ uname -a
Linux demoserver 5.4.0-100-generic #113-Ubuntu SMP Thu Feb 3 18:43:29 UTC 2022 x86_64 x86_64 x86_64 GNU/Linux
cardano_node@demoserver:~/coreapp_testing/new_repo$ cabal --version
cabal-install version 3.6.2.0
compiled using version 3.6.2.0 of the Cabal library
cardano_node@demoserver:~/coreapp_testing/new_repo$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
```

How to run
```
cardano run trace
```
