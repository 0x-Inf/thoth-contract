# System Specification

This document contains the specifications for the Thoth Network protocol.

The system is divided into two major parts. These include:
 -Thoth Network
 -Researcher State Machine

## Thoth Network Contract

In this section we describe the workings of the main network contract. The main function of this part is to 'cordinate' the other distributed parts of the
system. It will consist of:

- Functions that change the state.
- The inititalization Function.
- The data for the system
- Interface for Interacting (i.e a way for people to know the state)
- Validation for changes.

### Thoth Network Validation

The function to start the network protocol will be of the following signature: `networkValidator :: ResearcherPkh -> DatumType -> TypedRedeemer -> Context -> Bool`

Let's go over the various parameters:

| Parameter | Possible Values | Description |
|---|---|---|
| `ResearcherPKH` | `PaymentPubKeyHash` | This will be the public keyhash of the person who is initializing their Researcher page. This will also where the tokens minted during initialization would be send to. This pkh will also be used to parametrize a script which will hold the 'state' of the researcher in addition to parametrizing their thoth token |
| `DatumType` | `NetworkDatum` this will consists of  `[PaymentPubKeyHash]` $ \times $ `[Address]` $ \times $ `NetworkParams` $ \times $ `NetworkAddress` | The datum type will be the data type that represents the datum of the utxos in the network address. This will carry the 'state' of the network if it were a dynamical system. So any changes that affect the network wide position will be reflected by changing the datum. This will include the inititalization of a researcher page or initialization of the network itself. |
| `TypedRedeemer` | `()` $ \times $ `InitNetwork` $ \times $ `InitResearcher` $ \times $ `InitGuest` $ \times $ `ChangeNetworkParams` | This will act as the interface if we are using the analogy of the network being like a dynamical system. This will determine what changes will take place in the state of the network. Particulary in the initNetwork 'direction' the validator will ensure that there are no outputs in the contract address and if there are then we can't initialize an existing network (Maybe put this on a separate section). If that check passes then we also check that the outgoing outputs also contain the Initdatum. We also check that the pubkeyhash gets some tokens to show the effort among other tokens... The particulars of the other redeemers will be outlined below lest this table entry becomes longer|
| `Context` | `ScriptContext` := `TxInfo` $ \times $ `ScriptPurpose`  | According to the type documentation at time of writing, this will have the details of the pending transaction. This will include details of the transaction inputs, outputs, the fee paid, the value minted if any, the valid range , a map of DatumHash and Datum. There is also another context data constructor which specifies the purpose of the transaction. This could be Minting, Spending, Rewarding, Certifying. The particular purpose will depend on the redeemer provided and also if certain checks pass |
| `Bool` | `True` $ \times $ `False` | This is the output of the networkValidator, it will either be true and the transaction validated will be included in the block or false and this will lead to transaction failure. According to the alonzo era 2 phase transaction validation, this corresponds to the second phase and therefore some fees will be required for computational resources consumed in trying to validate the transaction.  |

The above is just an overall specification of what might happen when someone tries to interact with the thoth network.

We're now going to look at the particulars for the redeemer.

| RedeemerType | Required data | Checks | Description |
|----|----|----|----|
| `InitNetwork` | `PaymentPubKeyHash` $ \times $ `ScriptContext` | <ul><li>Ascertain that pkh is valid and has utxos to spend particularly ada</li><li>check that no datum exists in the network address or that it Maybe Nothing</li><li> Check that the output of the transaction contains some tokens of interest, specifically the effort tokens, the thoth network token and hence it's minting validator</li></ul> | This network interaction will be responsible for setting up network initial state. Only one transaction is allowed to do this. This 'transition' will create the datums for the network and also mint some tokens which will be paid to the script itself and also to the redeeming pkh. Since there is Just one possible transaction for this, we don't worry about network congestion that occurs when multiple users try to spend the same utxo. |
| `InitResearcher` | `PaymentPubKeyHash` $ \times $ `ScriptContext` $ \times $ `NetworkDatum` | <ul><li> The pkh is not present in the datum </li><li>The pkh doesn't contain thoth tokens</li><li>The pkh has enough to cover transaction fees and token transfers</li><li>The datum is present in the input utxo</li><li>The Network datum is present in the output utxo</li><li>One of the output utxos is a thoth token whose policy is parametrized by the pkh</li><li>Some network parameters have been updated, which will affect how certain tokens are minted</li><li> If the InitResearcher params contain 'references' and if so handle them using helper fns</li><li>The output datum is updated</li><li> There is a script address that the pkh has 'admin' priviladges on(This will correspond to the reasearcher page)</li</ul> | This is the redeemer many first time users would interect with and hence it has the potential to have congestion, to prevent this the script will contain multiple utxos at any time which can be used to initialize a researcher. Therefore any one user can randomly pick a utxo that is 'identical' to other utxos that other users may pick at the same time, the chances of trying to spend the same utxo are reduced but not eliminated. We might use a order book matching pattern to further make sure we avoid congestion|
| `InitGuest` | `PaymentPubKeyHash` $ \times $ `ScriptContext` $ \times $ `NetworkDatum` $ \times $ `TxOut` | <ul><li> check that pkh has enough ada for transaction and Maybe token tranfer minAmount </li><li> Check that the outgoing Utxo has updated Datum </li><li> check that the guest pkh gets some effort token(s) for this particular action </li><li>Items</li></ul>| This will be for visitors who don't want a permanent research page and Just want to view some content. This will allow them to get effort tokens as they can contribute and if so wishes mint some effort tokens and they'll appear at their address of choice. Having a guest pkh will also allow one to do most of other stuff the other users do on the network... not their pages |
| `MorphNetworkParameters` | `PaymentPubKeyHash` $\times$ `ScriptContext` $\times$ `MorphProposal` $\times$ `NetworkDatum` | <ul><li> check that the proposal has the number of votes required to change a parameter </li><li> Check that the pkh is the stated executor of proposal</li><li>Check that the outgoing utxo has the datum express the changes </li><li>Check that incoming outputs have datum</li><li> Mark the proposal as done (It might have been a state machine and this will 'close it')</li><li>Items</li></ul>| This action of the network will have to be ratified by all people who want to have a say and hence it has to be accompanied by a proposal that has been agreed on. The checks ensures that this happens and that the changes are now part of the new state of the network |

Now that we have an idea of the how the redeemer would influence what transactions are true or not. We can have a look at the fn that will make all that possible, here is a list of them with a short description and it's type signature.
