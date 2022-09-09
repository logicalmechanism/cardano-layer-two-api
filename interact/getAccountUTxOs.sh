echo
# User A
pkh='payload=00f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74'
echo $pkh
# exit

curl -X POST http://localhost:8000/entries/getUTxOs/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${pkh}"

echo
echo
# User B
pkh='payload=b5b5ae8e050355c7cc64415468c0ce135f44355c312cedf5474235f2'
echo $pkh
# exit
curl -X POST http://localhost:8000/entries/getUTxOs/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${pkh}"

echo
echo
# User C
pkh='payload=9bd662da31d27afa412d57d1705b7c0e3be4c5ef2e73c1b228c763d3'
echo $pkh
# exit

curl -X POST http://localhost:8000/entries/getUTxOs/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${pkh}"

echo
