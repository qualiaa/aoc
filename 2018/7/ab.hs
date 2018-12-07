import Data.Char (isUpper, ord)
import Data.List (minimumBy, sortBy, (\\))
import Data.Maybe (isJust, maybeToList)
--import qualified Data.Map as M
import Data.Ord (comparing)

-- do not write haskell when tired
-- state monad pls

-- Minutes Job
type Task    = Char
type DepList = [(Task, Task)]
data Worker  = Worker Int (Maybe Task) deriving (Show)

worker = Worker 0 Nothing
initWorkers = replicate 5 worker

duration task = (ord task) - (ord 'A') + 61

giveTask (Worker m Nothing) t = Worker m' $ Just t
    where m' = duration t + m

nextTasks list solved = filter (`notElem` (map snd list)) (['A'..'Z'] \\ solved)
nextTask l = head . nextTasks l
initTasks depList = nextTasks depList ""

solveTask :: DepList -> Task -> DepList
solveTask deps task = if task `notElem` (initTasks deps) then error "Uhoh"
    else filter (\(dep,_) -> dep /= task) deps

solveTasks :: DepList -> [Task] -> DepList
solveTasks = foldl solveTask 

depenendencyOrder :: DepList -> [Task]
depenendencyOrder l = reverse $ depenendencyOrder' l ""

    where depenendencyOrder' [] solved = (nextTasks [] solved) ++ solved
          depenendencyOrder' depList solved = depenendencyOrder' newList' solved'
            where newList' = solveTask depList task
                  task     = nextTask depList solved
                  solved'  = task:solved

assignedTasks :: [Worker] -> [Task]
assignedTasks = foldl (\ts (Worker _ t) -> maybeToList t ++ ts) []

assignWorkers :: Int -> [Worker] -> [Task] -> [Worker]
assignWorkers time workers tasks = snd $ foldl assignWorker (tasks',[]) workers

    where tasks' = tasks \\ (assignedTasks workers)
          assignWorker :: ([Task],[Worker]) -> Worker -> ([Task], [Worker])
          assignWorker ([],ws) w = ([],w:ws)
          assignWorker ((t:ts),ws) w@(Worker m Nothing) = (ts, (Worker (duration t + time) (Just t)):ws)
          assignWorker (ts,ws) w = (ts,w:ws)

nextSolvedTasks :: [Worker] -> (Int, [Task],[Worker])
nextSolvedTasks workers = (solvedTime, solvedTasks, nextWorkers)

    where (solvedTasks, nextWorkers) = foldl completeWorker ([], []) workers
          assignedWorkers = filter (\(Worker _ t) -> isJust t) workers
          solvedTime = minimum $ map (\(Worker m _) -> m) assignedWorkers
          completeWorker :: ([Task], [Worker]) -> Worker -> ([Task], [Worker])
          completeWorker (ts, ws) w@(Worker _ Nothing) = (ts, w:ws)
          completeWorker (ts, ws) w@(Worker m (Just t)) = 
                case m `compare` solvedTime of
                    LT -> error "Wuh wo"
                    EQ -> (t:ts, (Worker m Nothing):ws)
                    GT -> (ts, w:ws)

workerSolve :: DepList -> [Worker] -> [Worker]
workerSolve l ws = workerSolve' l ws "" 0

    where workerSolve' [] workers solved time = assignWorkers time workers (nextTasks [] solved)
          workerSolve' depList workers solved time =
                let assignedWorkers = assignWorkers time workers $ nextTasks depList solved
                    (time', solved', workers') = nextSolvedTasks assignedWorkers
                    depList' = solveTasks depList solved'
                in workerSolve' depList' workers' (solved' ++ solved) time'
        

{-
timeToSolve :: DepList -> Int
timeToSolve = maximum . map (\(Worker t _) -> t) . workerSolve
-}

main = do
    depList <- sortBy (comparing snd) .map (list2toTuple . filter isUpper . drop 1) . lines <$> getContents
    putStrLn $ depenendencyOrder depList
    print $ workerSolve depList initWorkers
    

list2toTuple [a,b] = (a,b)
