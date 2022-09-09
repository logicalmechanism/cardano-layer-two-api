# [
#     {
#         "inputs": {
#             "9f08bb4d4e4323cfa7fcd5f719329af1b28aa1be233f4c0fd4baab4b2e9f5d8d#0": {}
#         }, 
#         "outputs": {
#             "b5b5ae8e050355c7cc64415468c0ce135f44355c312cedf5474235f2": {"": {"": 4}}, 
#             "9bd662da31d27afa412d57d1705b7c0e3be4c5ef2e73c1b228c763d3": {"": {"": 5}}
#         }, 
#         "fee": 1
#     }, 
#     [
#         {
#             "pkh": "00f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74",
#             "data":"A366696E70757473A17842396630386262346434653433323363666137666364356637313933323961663162323861613162653233336634633066643462616162346232653966356438642330A0676F757470757473A278386235623561653865303530333535633763633634343135343638633063653133356634343335356333313263656466353437343233356632A160A1600478383962643636326461333164323761666134313264353764313730356237633065336265346335656632653733633162323238633736336433A160A160056366656501",
#             "sig": "845846a201276761646472657373583900f81595a5e215c4cc63ec82a0790e66c6b109033bc34e23c03cd756eb57e3e14dcee6ba8f48b97044ca868b4ee017d04ecc792de386beab74a166686173686564f458daa366696e70757473a17842396630386262346434653433323363666137666364356637313933323961663162323861613162653233336634633066643462616162346232653966356438642330a0676f757470757473a278386235623561653865303530333535633763633634343135343638633063653133356634343335356333313263656466353437343233356632a160a1600478383962643636326461333164323761666134313264353764313730356237633065336265346335656632653733633162323238633736336433a160a160056366656501584065fc110117fb2e814581fb6ae291aa9f095ae7749f54dda7ad770a62b7bdf3f7d16f792eb75365bbaa96ea925cef4f645fb5217caf039b1e3b83d8e9369d6201", 
#             "key": "a4010103272006215820682631f4e8e60860cb3db3870cc4e0697e8783535e0a455409cf2fc35a3754fd"
#         }
#     ], 
#     "always_succeed"
# ]

txCbor="83A366696E70757473A17842396630386262346434653433323363666137666364356637313933323961663162323861613162653233336634633066643462616162346232653966356438642330A0676F757470757473A278386235623561653865303530333535633763633634343135343638633063653133356634343335356333313263656466353437343233356632A160A1600478383962643636326461333164323761666134313264353764313730356237633065336265346335656632653733633162323238633736336433A160A16005636665650181A463706B68787230306638313539356135653231356334636336336563383261303739306536366336623130393033336263333465323363303363643735366562353765336531346463656536626138663438623937303434636138363862346565303137643034656363373932646533383662656162373464646174617901B441333636363936453730373537343733413137383432333936363330333836323632333436343334363533343333333233333633363636313337363636333634333536363337333133393333333233393631363633313632333233383631363133313632363533323333333336363334363333303636363433343632363136313632333436323332363533393636333536343338363432333330413036373646373537343730373537343733413237383338363233353632333536313635333836353330333533303333333533353633333736333633333633343334333133353334333633383633333036333635333133333335363633343334333333353335363333333331333236333635363436363335333433373334333233333335363633324131363041313630303437383338333936323634333633363332363436313333333136343332333736313636363133343331333236343335333736343331333733303335363233373633333036353333363236353334363333353635363633323635333733333633333136323332333233383633333733363333363433334131363041313630303536333636363536353031637369677902E038343538343661323031323736373631363436343732363537333733353833393030663831353935613565323135633463633633656338326130373930653636633662313039303333626333346532336330336364373536656235376533653134646365653662613866343862393730343463613836386234656530313764303465636337393264653338366265616237346131363636383631373336383635363466343538646161333636363936653730373537343733613137383432333936363330333836323632333436343334363533343333333233333633363636313337363636333634333536363337333133393333333233393631363633313632333233383631363133313632363533323333333336363334363333303636363433343632363136313632333436323332363533393636333536343338363432333330613036373666373537343730373537343733613237383338363233353632333536313635333836353330333533303333333533353633333736333633333633343334333133353334333633383633333036333635333133333335363633343334333333353335363333333331333236333635363436363335333433373334333233333335363633326131363061313630303437383338333936323634333633363332363436313333333136343332333736313636363133343331333236343335333736343331333733303335363233373633333036353333363236353334363333353635363633323635333733333633333136323332333233383633333733363333363433336131363061313630303536333636363536353031353834303635666331313031313766623265383134353831666236616532393161613966303935616537373439663534646461376164373730613632623762646633663764313666373932656237353336356262616139366561393235636566346636343566623532313763616630333962316533623833643865393336396436323031636B657978546134303130313033323732303036323135383230363832363331663465386536303836306362336462333837306363346530363937653837383335333565306134353534303963663266633335613337353466646E616C776179735F73756363656564"

data='{"number": 1, "cbor":"'${txCbor}'"}'
value=$(python3 -c "from cbor2 import dumps;print(dumps(${data}).hex())")
# echo $value
payload='payload='${value}
# echo $payload
# exit

curl -X POST http://localhost:8000/tasks/newTask/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${payload}"

echo
