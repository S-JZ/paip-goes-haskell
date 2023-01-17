import Data.IORef
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as L
import qualified Data.Text as DT

type State = String

makeInitialState = do
    userInput <- getInputFromUser "States"
    globalState <- newIORef userInput
    return globalState

getInputFromUser askUser = do
    putStrLn $ "Enter " ++ askUser
    userInputStates <- getLine
    return $ map (DT.unpack) $ DT.splitOn (DT.pack ", ") (DT.pack userInputStates)

prefix = "Executing"

data Operator = Operator {
                    action   :: String,
                    preconds :: [String],
                    toAdd    :: [String],
                    toDelete :: [String]
                } deriving Show

operators = [
           Operator {
                    action = "drive son to school", 
                    preconds = ["son at home", "car works"], 
                    toAdd = ["Executing drive son to school", "son at school"], 
                    toDelete = ["son at home"]
                    },
           Operator {
                    action = "shop installs battery",
                    preconds = ["car needs battery", "shop knows problem", "shop has money"],
                    toAdd = ["Executing shop installs battery", "car works"],
                    toDelete = []
                    },
           Operator {
                    action = "tell shop problem", 
                    preconds = ["in communication with shop"],
                    toAdd = ["Executing tell shop problem", "shop knows problem"],
                    toDelete = []
                    },
           Operator {
                    action = "telephone shop", 
                    preconds = ["know phone number"],
                    toAdd = ["Executing telphone shop", "in communication with shop"],
                    toDelete = []
                    },
           Operator {
                    action = "look up number", 
                    preconds = ["have phone book"],
                    toAdd = ["Executing look up number", "know phone number"],
                    toDelete = []
                    },
           Operator {           
                    action = "give shop money", 
                    preconds = ["have money"],
                    toAdd = ["Executing give shop money", "shop has money"],
                    toDelete = ["have money"]
                    }  
           ]

revOperators = reverse operators

goalStack = []
 
gps states goals operators = do
    achievedState' <- achieveAll states operators goals []
    case achievedState' of 
         Nothing -> do 
             return []
         Just achievedState -> do
             achievedStateArr <- readIORef achievedState
             return $ filter (\action -> L.isPrefixOf prefix action) achievedStateArr

achieveAll states operators [] goalStack = do
    return $ Just states
achieveAll states operators goals goalStack = do
    achievedGoal <- achieve states operators (head goals) goalStack
    if (null achievedGoal)
        then do 
             return Nothing
        else do
             writeIORef states achievedGoal
             result <- achieveAll states operators (tail goals) goalStack
             return result

checkGoalInStates goal states = do
    states' <- readIORef states
    return $ checkGoalInArr goal states'

checkGoalInArr goal arr = goal `elem` arr

achieve states operators goal goalStack = do 
    check <- checkGoalInStates goal states
    case check of 
        True ->  do
           states' <- readIORef states
           return states'
        False -> do
           if (checkGoalInArr goal goalStack)
               then do
                    return [] 
               else do
                    result <- isAppropriateOperator states operators goal goalStack
                    return result

isAppropriateOperator states [] goal goalStack = do
    return []
isAppropriateOperator states operators goal goalStack = do
    let op = head operators
    if (checkGoalInArr goal $ toAdd op)
        then do  
            result <- applyOperator op states operators goal goalStack
            if (null result) 
                then isAppropriateOperator states (tail operators) goal goalStack
                else 
                    return result
        else isAppropriateOperator states (tail operators) goal goalStack

elemNotInList elem lst = elem `notElem` lst
statesNotDeleted lst states = do
    statesArr <- readIORef states
    return $ filter (\x -> (elemNotInList x lst)) statesArr

applyOperator operator states operators goal goalStack = do
    achieveAllResult <- achieveAll states operators (preconds operator) ([goal] ++ goalStack)
    case achieveAllResult of 
        Nothing -> do 
            return []
        Just result -> do
            _result <- readIORef result
            let addList = toAdd operator
                deleteList = toDelete operator
            filteredResult <- statesNotDeleted deleteList result
            return $ filteredResult ++ addList

main = do
     states <- makeInitialState
     readStatesArr <- readIORef states 
     goals <- getInputFromUser "Goals"
     finalStates <- gps states goals operators
     putStrLn $ L.intercalate "\n" finalStates
