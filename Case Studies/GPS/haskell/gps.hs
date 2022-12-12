import Data.IORef
import qualified Data.Foldable as F

makeInitialState = do
    globalState <- newIORef ["world"]
    return globalState

readGlobalState globalState = do x <- readIORef globalState
                                 return x

goals = ["Hello, World", "Another One"]

data Operator = Operator {
                    action   :: String,
                    preconds :: [String],
                    toAdd    :: [String],
                    toDelete :: [String]
                } deriving Show

operators = [
           Operator {
                    action = "say hello",
                    preconds = ["world"],
                    toAdd = ["Executing say hello", "Hello, World"],
                    toDelete = ["world"]
                    }
            ]

prefix = "Executing "

gps states goals operators = do
    let finalState = achieveAll states operators goals []
    return $ readIORef finalState

achieveAll states operators goals goalStack = achieve states operators goals goalStack

achieve states operators goals goalStack = applyOperator (head operators) states operators (head goals) goalStack

applyOperator operator states operators goal goalStack = states
