set olddir=%CD%

cd ..\..\build\Debug\

grassDTmodel.exe %olddir%\lat51.391900_lon11.878700__2013-01-01_2022-12-31__configuration__generic_v1.txt 1> %olddir%\stout.txt 2> %olddir%\sterr.txt








