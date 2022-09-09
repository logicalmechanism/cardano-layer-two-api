
data='{"pkh": "00f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74","utxos": {"9f08bb4d4e4323cfa7fcd5f719329af1b28aa1be233f4c0fd4baab4b2e9f5d8d#0": {"": {"": 10}}}}'
value=$(python3 -c "from cbor2 import dumps;print(dumps(${data}).hex())")
payload='payload='${value}
# echo $value
# echo $payload
# exit

curl -X POST http://localhost:8000/entries/newUTxO/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${payload}"

echo