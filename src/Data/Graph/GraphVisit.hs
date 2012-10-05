{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

------------------------------------------------------------------------------
-- | This module provides a graph visiting abstraction
module Data.Graph.GraphVisit
  ( 
    -- * Graph visiting
    graphVisitM
  , graphVisit
  
    -- * Result
  )
  where

------------------------------------------------------------------------------
import qualified Data.Set as Set
import           Control.Monad.State.Strict
import           Data.Lens.Common
import           Data.Lens.Strict
import           Data.Lens.Template
------------------------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | The state employed by visiting
data GraphVisitState node thr = GraphVisitState
  { _gvsVisited		:: Set.Set node		-- ^ visited nodes
  , _gvsYetToVisit	:: Set.Set node		-- ^ to be visited nodes
  , _gvsAccum		:: thr				-- ^ threaded accumulator
  }

makeLens ''GraphVisitState

------------------------------------------------------------------------------
-- | Abstract graph visit, over arbitrary structures, using state holding visited nodes (also acting als start) and an accumulator 'thr'.
-- All is done in strict StateT.
graphVisitOnStateM
  :: (Ord node, Monad m)
     => (thr -> graph -> node -> m (thr,Set.Set node))      -- ^ fun: visit node, get new thr and nodes to visit next
     -> graph                                               -- ^ graph over which we visit
     -> StateT (GraphVisitState node thr) m ()
graphVisitOnStateM visit graph
  = v
  where v = do
          s <- get
          unless (Set.null $ s ^. gvsYetToVisit) $ do
            let (n, unvisited) 		= Set.deleteFindMin $ s ^. gvsYetToVisit
            (thr, newUnvisited)    <- lift $ visit (s ^. gvsAccum) graph n
            let newAndOldVisited 	= Set.insert n $ s ^. gvsVisited
            gvsVisited    != newAndOldVisited
            gvsYetToVisit != Set.union (newUnvisited `Set.difference` newAndOldVisited) unvisited
            gvsAccum      != thr
            v

------------------------------------------------------------------------------
-- | Abstract monadic graph visit, over arbitrary structures, using state holding visited nodes (also acting als start) and an accumulator 'thr'.
graphVisitM
  :: (Ord node, Monad m)
     => (thr -> graph -> node -> m (thr,Set.Set node))      -- ^ fun: visit node, get new thr and nodes to visit next
     -> Set.Set node                                        -- ^ root/start
     -> graph                                               -- ^ graph over which we visit
     -> thr                                                 -- ^ the accumulator, threaded as state
     -> m (thr,Set.Set node)                                -- ^ yield accum and visited
graphVisitM visit start graph thr = do
  s <- execStateT (graphVisitOnStateM visit graph) (GraphVisitState Set.empty start thr)
  return (s ^. gvsAccum, s ^. gvsVisited)

------------------------------------------------------------------------------
-- | Abstract graph visit, running graphVisitM
graphVisit
  :: (Ord node)
     => (thr -> graph -> node -> (thr,Set.Set node))        -- ^ fun: visit node, get new thr and nodes to visit next
     -> Set.Set node                                        -- ^ root/start
     -> graph                                               -- ^ graph over which we visit
     -> thr                                                 -- ^ the accumulator, threaded as state
     -> (thr,Set.Set node)                                  -- ^ yield accum and visited
graphVisit visit start graph thr
  = (s ^. gvsAccum, s ^. gvsVisited)
  where s = execState (graphVisitOnStateM (\t g n -> return $ visit t g n) graph) (GraphVisitState Set.empty start thr)
