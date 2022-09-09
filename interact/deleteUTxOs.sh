
data='{"pkh": "b5b5ae8e050355c7cc64415468c0ce135f44355c312cedf5474235f2","utxos": ["396d647ea80069b413c0d1b1523bb0c5c852c7aa00ad80a0c4f919130347b69f#0"]}'
value=$(python3 -c "from cbor2 import dumps;print(dumps(${data}).hex())")
payload='payload='${value}

# echo $pkh
# exit

curl -X POST http://localhost:8000/entries/deleteUTxOs/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${payload}"

echo

data='{"pkh": "9bd662da31d27afa412d57d1705b7c0e3be4c5ef2e73c1b228c763d3","utxos": ["396d647ea80069b413c0d1b1523bb0c5c852c7aa00ad80a0c4f919130347b69f#1"]}'
value=$(python3 -c "from cbor2 import dumps;print(dumps(${data}).hex())")
payload='payload='${value}

# echo $pkh
# exit

curl -X POST http://localhost:8000/entries/deleteUTxOs/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${payload}"

echo