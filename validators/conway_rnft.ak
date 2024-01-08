use aiken/transaction/credential.{Credential, ScriptCredential}

// Helper types

type Script =
  ByteArray

type VerificationKey =
  ByteArray

type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

type PolicyId =
  Hash<Blake2b_224, Script>

// Contract types

type RnftRedeemer {
  Commit
  Mint
}

type RnftDatum {
  commitment_block: Int,
  signer: PublicKeyHash,
}

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

// Conway RNFT smart contract

const tuna_address: ScriptCredential = #""

const tuna_policyid: PolicyId = #""

const tuna_asset_name: ByteArray = #""

validator(donation_address: ByteArray) {
  fn mint(rdm: RnftRedeemer, ctx: ScriptContext) {
    expect ScriptContext { transaction: tx, purpose: Mint(own_policy_id) } = ctx
    let Transaction { inputs, reference_inputs, outputs, fee, mint, .. } = tx

    when rdm is {
      Commit -> {
        // Find outputs sent to conway script address
        expect [output_to_conway] =
          find_script_outputs(ouputs, ScriptCredential(own_policy_id))

        // Get tunas sent to conway script address
        let comision: Int =
          quantity_of(output_to_conway.value, tuna_policyid, tuna_asset_name)

        // Expect an Inline datum sent to conway script address
        expect InlineDatum(out_datum) = output_to_conway.datum
        expect RnftDatum { commitment_block, signer } = out_datum

        // From the reference inputs find one that comes from the tuna script address
        expect [tuna_ref_input] =
          list.filter(
            ref_input,
            fn(input) {
              input.output.address.payment_credential == tuna_policyid
            },
          )

        // Expect from the Tuna reference input an InlineDatum of type State
        expect InlineDatum(tuna_datum) = tuna_ref_input.output.datum
        expect State { block_number: current_block_number, .. }: State =
          tuna_datum

        // Expect one token minted (Commitment token)
        expect [(asset_name, amount)] =
          mint
            |> value.from_minted_value
            |> value.tokens(own_policy_id)
            |> dict.to_list()

        and {
          // Are there tuna sent to the conway contract as comission ?
          comision == 10,
          // Is the commited block greatear than the current tuna block number ?
          current_block_number < commitment_block,
          // Does the out datum include a signature ?
          list.has(transaction.extra_signatories, signer),
          // Is just one commitment token ? 
          asset_name == #"000000" && amount == 1,
        }
      }

      Mine -> {
        expect ScriptContext { transaction: tx, purpose: Mint(own_policy) } =
          ctx

        let own_credential = ScriptCredential(own_policy)

        // Mint(0) Mine requirement: Contract has one spend input with the policy as the payment credential
        // If the spend part of the multivalidator is correct then you can mint the Rnft.
        list.any(
          tx.inputs,
          fn(input) {
            input.output.address.payment_credential == own_credential
          },
        )?
      }
    }
  }

  fn spend(dat: RnftDatum, rdm: Void, ctx: ScriptContext) {
    todo
    // 
  }
}

// Helper functions

// Get all outputs value
fn outputs_value(ouputs: List<Output>) -> Value {
  let ouputs = list.map(ouputs, fn(o) { o.value })
  list.foldr(
    ouputs,
    value.zero(),
    fn(output1, output2) { value.merge(output1, output2) },
  )
}
