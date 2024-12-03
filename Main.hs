import Data.Text
import Day1 qualified as D1
import Day2 qualified as D2
import System.Environment
import System.Exit
import System.IO

main = getArgs >>= parse

readInput day = readFile ("./inputs/" ++ day ++ ".txt")

chooseDay day
  | day == "1" = D1.part
  | day == "2" = D2.part
  | otherwise = error ("Not implemented day" ++ show otherwise)

exit = exitSuccess

parse ["-d", day, "-p", part] = readInput day >>= \input -> print (chooseDay day part input) >> exit
parse [] = print "empty input" >> exit
parse x = error ("Invalid Input: " ++ show x) >> exit
