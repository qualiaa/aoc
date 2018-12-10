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
    time <- currentTime

    let assignWorker :: Worker -> TaskState Worker
        assignWorker w@(Just _) = return w
        assignWorker Nothing = do
            todo <- readyTasks
            case todo of []    -> return Nothing
                         (t:_) -> return $ Just (t, (time + duration t))

    setWorkers =<< (join $ sequence . map assignWorker <$> workers)
          

finishAssignedTask :: TaskState ()
finishAssignedTask = do
    time <- minimum . map snd . catMaybes <$> workers
    setTime time

    let finishWorker :: Worker -> TaskState Worker
        finishWorker Nothing = return Nothing
        finishWorker w@(Just (task, t)) =
            if time == t then finishTask task >> return Nothing
                         else return w

    setWorkers =<< (join $ sequence . map finishWorker <$> workers)

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
