module TaskState
( Task
, Worker
, DependencyList
, Time
, TaskState

-- state ctor
, initState

-- convenience
, finishTask
, assignedTasks
, completableTasks
, readyTasks

-- accessors
, allTasks
, finishedTasks
, dependencyList
, workers
, currentTime
, setDependencyList
, setFinishedTasks
, setWorkers
, setTime
) where

import Control.Monad (ap, liftM)
import Control.Monad.State.Strict (State, get, modify)
import Data.List (nub, sort, (\\))
import Data.Maybe (catMaybes)
import qualified Data.Map as M

type Time           = Int
type Task           = Char
type Worker         = (Maybe (Task, Time))
type DependencyList = M.Map Task [Task]

data TaskState' = TaskState' { tasks           :: [Task]
                             , finished        :: [Task]
                             , dependencyList' :: DependencyList
                             , workers'        :: [Worker]
                             , time'           :: Time
                             } deriving (Show)

type TaskState a = State TaskState' a

initState :: DependencyList -> Int -> TaskState'
initState list nw = TaskState' { tasks           = sort . nub $ M.keys list ++
                                                       (concat $ M.elems list)
                               , finished        = []
                               , dependencyList' = list
                               , workers'        = replicate nw Nothing
                               , time'           = 0
                               }

-- useful functions
finishTask :: Task -> TaskState ()
finishTask t = do
    liftM (t `elem`) completableTasks >>= (\b ->
         if b then return () else error "uh oh")
    setFinishedTasks  =<< (t:) <$> finishedTasks
    setDependencyList =<< liftM (M.delete t) dependencyList 

completableTasks :: TaskState [Task]
completableTasks = do
    finished  <- finishedTasks
    dependent <- nub . concat . M.elems <$> dependencyList
    liftM (\\) allTasks `ap` return (finished ++ dependent)

readyTasks :: TaskState [Task]
readyTasks = liftM (\\) completableTasks `ap` assignedTasks

assignedTasks :: TaskState [Task]
assignedTasks = map fst . catMaybes <$> workers

-- state accessor boilerplate
allTasks       :: TaskState [Task]
finishedTasks  :: TaskState [Task]
dependencyList :: TaskState DependencyList
workers        :: TaskState [Worker]
currentTime    :: TaskState Time

setDependencyList :: DependencyList -> TaskState ()
setFinishedTasks  :: [Task]         -> TaskState ()
setWorkers        :: [Worker]       -> TaskState ()
setTime           :: Time           -> TaskState ()

allTasks       = liftM tasks get
finishedTasks  = liftM finished get
dependencyList = liftM dependencyList' get
workers        = liftM workers' get
currentTime    = liftM time' get

setDependencyList l = modify $ \st -> st { dependencyList' = l }
setFinishedTasks ts = modify $ \st -> st { finished = ts }
setWorkers ws       = modify $ \st -> st { workers' = ws }
setTime t           = modify $ \st -> st { time' = t }
