# abapjson
sap abap DDIC to json


step 1

You need  se24 create a class name value is zcl_ddic_to_json

copy my demo into your class save and active!

step 2
test class zcl_ddic_to_json
open sy-tcode: se24 

![image](https://github.com/XW-bmw/abapjson/assets/29417134/9eb2d12d-a0c5-4d03-b8f9-bb0f127b73f2)


Updated to add one of the switch iv_output parameters, when this is X, the JSON is displayed, the other parameter is IV_TCHECK, when this parameter is X, e.g. pa0001 will get the [{}] format json

![image](https://github.com/XW-bmw/abapjson/assets/29417134/f1adf2b6-cbdd-42cc-929e-25327015b2a9)



You'll see the presentation.


Special thanks to my friend Mud for providing test cases and assisting me with testing and code improvements.


