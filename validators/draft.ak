use aiken/bytearray
use aiken/cbor
use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{Credential, ScriptCredential}
use aiken/transaction/value.{Value}
use randr/prng.{hex_to_dec, icg_sequence, range_sequence, to_bytearray}

type Script =
  ByteArray

type VerificationKey =
  ByteArray

type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

type PolicyId =
  Hash<Blake2b_224, Script>

type PaymentCredential =
  Credential

type State {
  block_number: Int,
  current_hash: ByteArray,
  leading_zeros: Int,
  difficulty_number: Int,
  epoch_time: Int,
  current_posix_time: Int,
  extra: Data,
  interlink: List<Data>,
}

type RnftDatum {
  current_block_number: Int,
  minted_quantity: Int,
}

type MintingState {
  Mint
  Setup
}

validator {
  fn mint(rdm: MintingState, ctx: ScriptContext) -> Bool {
    // Set tunita vars...
    let tuna_address: PaymentCredential = ScriptCredential(#"")
    let tuna_policyid: PolicyId = #""
    let tuna_asset_name: ByteArray = #""
    let donation_address: ByteArray = #""
    let master_token_name = #""
    let genesis_output_ref =
      OutputReference {
        transaction_id: TransactionId { hash: #"" },
        output_index: 0,
      }

    // Get script context
    let ScriptContext { transaction, purpose } = ctx
    expect transaction.Mint(policy_id) = purpose
    // Get the minted value and the tuna datum as reference input. 
    let Transaction { inputs, reference_inputs, outputs, mint, .. } =
      transaction

    when rdm is {
      Mint -> {
        // Expect a list with just one element from the reference input
        expect [ref_input] = reference_inputs
        let Output { address, value, datum, .. } = ref_input.output

        // Cond 1: Does the address of the reference input matches the tuna script address?
        let is_tuna_address: Bool = address.payment_credential == tuna_address

        // Extract current hash and posix time from the tuna script datum
        expect InlineDatum(state_datum) = datum
        expect State { block_number, current_hash, current_posix_time, .. }: State =
          state_datum

        // Concat time and current hash to use as a digest, then convert it to decimal.
        let time = cbor.serialise(current_posix_time)
        let digest =
          bytearray.concat(time, current_hash)
            |> hash.sha2_256()
            |> hex_to_dec()

        // Take the digest as a PRNG seed.
        let random_name =
          icg_sequence([digest], 33)
            |> list.drop(1)
            |> range_sequence(0, 255)
            |> to_bytearray(#"", _)

        // Extract the minted value that correspond to the random assetname.
        expect [(_, token_name, minted_amount)] =
          value.from_minted_value(mint)
            |> value.flatten()
            |> list.filter(fn(match) { match.2nd == random_name })

        // Get value to paid to donation address
        let paid_to_donation: Value = value_paid_to(outputs, donation_address)

        // Check if 10 tunitas were sent to donation address.
        let tuna_amount =
          value.quantity_of(paid_to_donation, tuna_policyid, tuna_asset_name)

        and {
          is_tuna_address,
          token_name == random_name,
          minted_amount == 1,
          tuna_amount == 10,
        }
      }
      Setup -> {
        // Expect that input has specified UTxO
        expect Some(_genesis_input) =
          transaction.find_input(inputs, genesis_output_ref)

        // From the minted values expect just one token minted. 
        expect [(minted_asset_name, minted_amount)] =
          mint
            |> value.from_minted_value
            |> value.tokens(policy_id)
            |> dict.to_list()

        // Find outputs to this script address
        expect [own_output] =
          transaction.find_script_outputs(outputs, policy_id)

        // Get the value that goes to the script address
        expect [(ouput_policy_id, output_assetname, output_amount)] =
          outputs_value([own_output])
            |> value.without_lovelace()
            |> value.flatten()

        and {
          minted_asset_name == master_token_name,
          minted_amount == 1,
          policy_id == ouput_policy_id,
          minted_asset_name == output_assetname,
          minted_amount == output_amount,
        }
      }
    }
  }

  fn spend(dat: RnftDatum, rdm: Void, ctx: ScriptContext) {
    // TUNA Vars
    let tuna_address: PaymentCredential = ScriptCredential(#"")
    let master_token_name = #""

    // Get script context
    let ScriptContext { transaction, purpose } = ctx

    expect Spend(output_ref) = purpose

    // Get the minted value and the tuna datum as reference input. 
    let Transaction { inputs, reference_inputs, outputs, mint, .. } =
      transaction

    // Expect a list with just one element from the reference input
    expect [ref_input] = reference_inputs
    let Output { address, value, datum, .. } = ref_input.output

    // Cond 1: Does the address of the reference input matches the tuna script address?
    let is_tuna_address: Bool = address.payment_credential == tuna_address

    // Extract current hash and posix time from the tuna script datum
    expect InlineDatum(state_datum) = datum
    expect State { block_number, current_hash, current_posix_time, .. }: State =
      state_datum

    // Get inputs from this speding script
    let own_input_value: Value = [find_own_input(ctx)] |> inputs_value()
    // Get outputs to this spending script
    let own_output_values: Value = find_own_outputs(ctx) |> outputs_value()

    // Falta encontrar el script address que sea igual al policy ID!!!
    let has_master_token: Bool =
      value.quantity_of(
        own_input_value,
        output_ref.transaction_id,
        master_token_name,
      ) == 1

    let return_master_token: Bool =
      value.quantity_of(
        own_output_values,
        output_ref.transaction_id,
        master_token_name,
      ) == 1
    // Find input from this script address
    let own_input: Input = find_own_input(ctx)
    // Expect inline RnftDatum as input
    expect InlineDatum(script_datum) = own_input.output.datum
    expect RnftDatum {
      current_block_number: last_block_commited,
      minted_quantity: nrft_amount,
    }: RnftDatum = script_datum

    // Expect output ot the script address
    expect [own_output] = find_own_outputs(ctx)
    // Expect inline RnftDatum as output
    expect InlineDatum(output_datum) = own_output.datum
    expect RnftDatum {
      current_block_number: new_last_block_commited,
      minted_quantity: new_nfrt_amount,
    }: RnftDatum = output_datum
    // Check if the mint counter is incremented 
    let is_nfrt_count_incremented = nrft_amount == new_nfrt_amount + 1
    // Check if new datum updates the last block commited
    let update_last_block_commited = new_last_block_commited == block_number

    and {
      is_tuna_address,
      has_master_token,
      return_master_token,
      block_number > last_block_commited,
      is_nfrt_count_incremented,
      update_last_block_commited,
    }
  }
}

// Helper functions

// Get all inputs value, identical to PlutusTx valueSpent function.
fn inputs_value(inputs: List<Input>) -> Value {
  let inputs_ = list.map(inputs, fn(i) { i.output.value })
  list.foldr(inputs_, zero(), fn(input1, input2) { merge(input1, input2) })
}

// Get value paid to a public key hash.
fn value_paid_to(ouputs: List<Output>, receiver: PublicKeyHash) -> Value {
  let receiver_address = credential.from_verification_key(receiver)
  expect [match] = list.filter(ouputs, fn(o) { o.address == receiver_address })
  match.value
}

// Get input from the script.
fn find_own_input(ctx: ScriptContext) -> Input {
  expect Spend(output_ref) = ctx.purpose

  expect Some(input) =
    ctx.transaction.inputs
      |> transaction.find_input(output_ref)
  input
}

// Get outputs to the script.
fn find_own_outputs(ctx: ScriptContext) -> List<Output> {
  expect Spend(output_ref) = ctx.purpose

  let own_outputs =
    ctx.transaction.outputs
      |> transaction.find_script_outputs(output_ref.transaction_id.hash)
  own_outputs
}

// Get all outputs value
fn outputs_value(ouputs: List<Output>) -> Value {
  let ouputs = list.map(ouputs, fn(o) { o.value })
  list.foldr(
    ouputs,
    value.zero(),
    fn(output1, output2) { value.merge(output1, output2) },
  )
}
