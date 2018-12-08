import Control.Monad (ap, liftM, join)
import Control.Monad.State.Strict (evalState)
import Data.Char (isUpper, ord)
import Data.List (foldl', sort)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import TaskState

performTasksAlone :: TaskState [Task]
performTasksAlone = do
    todo <- completableTasks
    case todo of
        (t:_) -> finishTask t >> performTasksAlone
        []    -> reverse <$> finishedTasks

assignTasks :: TaskState ()
assignTasks = do
    todo <- readyTasks
    time <- currentTime
    let assignWorker ([],ws) w          = ([], w:ws)
        assignWorker (ts,ws) w@(Just _) = (ts, w:ws)
        assignWorker (t:ts,ws) Nothing  = 
            let w = Just (t, (time + duration t)) in (ts, w:ws)

    liftM (snd . foldl' assignWorker (todo,[])) workers >>= setWorkers
          

finishWorkers :: TaskState ()
finishWorkers = do
    time <- currentTime
    let finishWorker :: Worker -> TaskState Worker
        finishWorker Nothing = return Nothing
        finishWorker w@(Just (task, t)) =
            if time /= t then return w else finishTask task >> return Nothing
    setWorkers =<< (join $ sequence . map finishWorker <$> workers)
                
finishAssignedTask :: TaskState ()
finishAssignedTask = do
    minimum . map snd . catMaybes <$> workers >>= setTime
    finishWorkers
    

minDuration = 60
duration :: Task -> Time
duration t = ord t - ord 'A' + 1 + minDuration

performTasksWithWorkers :: TaskState Time
performTasksWithWorkers = do
    finished <- liftM (==) (sort <$> finishedTasks) `ap` allTasks
    if finished
        then currentTime
        else assignTasks >> finishAssignedTask >> performTasksWithWorkers

main = do
    depList <- M.fromListWith (++) .
        map (fmap (:[]) . list2toPair . filter isUpper . drop 1) .  lines <$> getContents
    let initialState = initState depList 5

    putStrLn $ evalState performTasksAlone initialState
    print $ evalState performTasksWithWorkers initialState


list2toPair [a,b] = (a,b)
