module SaveState where
import Snowflake
dep_command     = Nothing
old_dep_command = Nothing
new_command     = Nothing
last_programs   = []
translations :: [(Polarity, Int, [String])]
translations = [(Pos,1,["literal"]),(Neg,1,["illiteral"]),(Pos,2,["fuse"]),(Neg,2,["defuse"]),(Pos,3,["summon"]),(Neg,3,["banish"]),(Pos,4,["fork"]),(Neg,4,["spoon"]),(Pos,5,["hokey"]),(Neg,5,["cokey"]),(Pos,6,["kitten"]),(Neg,6,["antikitten"])]
