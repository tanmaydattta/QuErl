-module(dateUtils_tests).
-include_lib("eunit/include/eunit.hrl").
day_difference_test()->
 365 =dateUtils:day_difference({2012,1,1},{2012,12,31}).
conventions_test()->
 0.15846994535519127 = dateUtils:conventions('act/act_isda',{2000,01,01}, {2000,02,28}),
 0.00273224043715847= dateUtils:conventions('act/act_isda',{2000,02,28}, {2000,02,29}),
 0.00546448087431694 = dateUtils:conventions('act/act_isda',{2000,02,28}, {2000,03,01}),
 0.00273224043715847= dateUtils:conventions('act/act_isda',{2000,02,29}, {2000,03,01}),
 0.997716894977169 = dateUtils:conventions('act/act_isda',{2000,03,01}, {2001,03,01}),
 0.5 = dateUtils:conventions('act/act_isda',{2000,06,15}, {2000,12,15}),
 5.002283105022832 = dateUtils:conventions('act/act_isda',{1999,03,01}, {2004,03,01}),
 6.0 = dateUtils:conventions('act/act_isda',{1999,03,01}, {2005,03,01}),
 % 0.495890411 = dateUtils:conventions('act/act_isda',{1999,01,15}, {1999,07,15}),
 % 0.002739726 = dateUtils:conventions('act/act_isda',{2001,01,30}, {2001,01,31}),
 % 0.252054795 = dateUtils:conventions('act/act_isda',{2001,05,31}, {2001,08,31}),
 % 0.00273224 = dateUtils:conventions('act/act_isda',{2000,02,28}, {2000,02,29}),
 % 1.002298076 = dateUtils:conventions('act/act_isda',{1999,02,28}, {2000,02,29}),
 % 0.997701924 = dateUtils:conventions('act/act_isda',{2000,02,29}, {2001,02,28}),
 1.0 = dateUtils:conventions('act/act_isda',{2001,02,28}, {2002,02,28}),
 % 0.249315068 = dateUtils:conventions('act/act_isda',{2001,05,31}, {2001,08,30}),
 % 0.252054795 = dateUtils:conventions('act/act_isda',{2001,05,30}, {2001,08,30}),
 4.0 = dateUtils:conventions('act/act_isda',{2000,02,29}, {2004,02,29}).
round(Number, Precision) ->
 P = math:pow(10, Precision),
 round(Number * P) / P.
 %04 0.252054795 0.249315068 1 0.997701924 1.002298076 0.00273224 0.252054795 0.002739726 0.495890411 6 5.002290591 0.5 0.997709409 0.00273224 0.005464481 0.00273224 .158469945



