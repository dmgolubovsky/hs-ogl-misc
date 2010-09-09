{- 
    Copyright 2008-2009 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, FlexibleContexts, FlexibleInstances #-}

-- | This module can be used to optimize any complex computation that can be broken down into parallelizable
-- sub-computations. The computations in question may be pure values, monadic values, list or stream transformations or
-- anything else provided that it's parallelizable and has a relatively predictable computation cost. Each elementary
-- sub-computation needs to be packaged as a 'Component' using the constructor 'atomic'. Sub-computations can then be
-- combined into larger computations using the other constructors.

module Control.Concurrent.Configuration
   (-- * The Component type
    Component (..),
    -- * Utility functions
    showComponentTree,
    -- * Constructors
    atomic, lift, liftParallelPair, liftSequentialPair, parallelRouterAndBranches, recursiveComponentTree
    )
where

import Data.List (minimumBy)

-- | 'AnyComponent' is an existential type wrapper around a 'Component'.
data AnyComponent = forall a. AnyComponent {component :: Component a}

-- | A 'Component' carries a value and metadata about the value. It can be configured to use a specific number of
-- threads.
data Component c = Component {
   -- | Readable component name.
   name :: String,
   -- | Returns the list of all children components.
   subComponents :: [AnyComponent],
   -- | Returns the maximum number of threads that can be used by the component.
   maxUsableThreads :: Int,
   -- | Configures the component to use the specified number of threads. This function affects 'usedThreads', 'cost',
   -- and 'subComponents' methods of the result, while 'name' and 'maxUsableThreads' remain the same.
   usingThreads :: Int -> Component c,
   -- | The number of threads that the component is configured to use. The default number is usually 1.
   usedThreads :: Int,
   -- | The cost of using the component as configured. The cost is a rough approximation of time it would take to do the
   -- job given the 'usedThreads'.
   cost :: Int,
   -- | The content.
   with :: c
   }

-- | Show details of the given component's configuration.
showComponentTree :: forall c. Component c -> String
showComponentTree c = showIndentedComponent 1 c

showIndentedComponent :: forall c. Int -> Component c -> String
showIndentedComponent depth c = showRightAligned 4 (cost c) ++ showRightAligned 3 (usedThreads c) ++ replicate depth ' '
                                ++ name c ++ "\n"
                                ++ concatMap (showIndentedAnyComponent (succ depth)) (subComponents c)

showIndentedAnyComponent :: Int -> AnyComponent -> String
showIndentedAnyComponent depth (AnyComponent c) = showIndentedComponent depth c

showRightAligned :: Show x => Int -> x -> String
showRightAligned width x = let str = show x
                           in replicate (width - length str) ' ' ++ str

data ComponentConfiguration = ComponentConfiguration {componentChildren :: [AnyComponent],
                                                      componentThreads :: Int,
                                                      componentCost :: Int}

-- | Function 'toComponent' takes a component name, maximum number of threads it can use, and its 'usingThreads'
-- method, and returns a 'Component'.
toComponent :: String -> Int -> (Int -> (ComponentConfiguration, c)) -> Component c
toComponent name maxThreads usingThreads = usingThreads' 1
   where usingThreads' n = let (configuration, c') = usingThreads n
                           in Component name (componentChildren configuration) maxThreads usingThreads'
                                        (componentThreads configuration) (componentCost configuration) c'

-- | Function 'atomic' takes the component name and its cost creates a single-threaded component with no subcomponents.
atomic :: String -> Int -> c -> Component c
atomic name cost x = toComponent name 1 (\_threads-> (ComponentConfiguration [] 1 cost, x))

-- | Function 'optimalTwoAlternatingConfigurations' configures two components that are meant to alternate in processing
-- of the data stream.
optimalTwoAlternatingConfigurations :: Int -> Component c1 -> Component c2
                                    -> (ComponentConfiguration, Component c1, Component c2)
optimalTwoAlternatingConfigurations threads c1 c2 = (cfg{componentCost= componentCost cfg `div` 2}, c1', c2')
   where (cfg, c1', c2') = optimalTwoSequentialConfigurations threads c1 c2


-- | Function 'optimalTwoParallelConfigurations' configures two components, both of them with the full thread count, and
-- returns the components and a 'ComponentConfiguration' that can be used to build a new component from them.
optimalTwoSequentialConfigurations :: Int -> Component c1 -> Component c2
                                   -> (ComponentConfiguration, Component c1, Component c2)
optimalTwoSequentialConfigurations threads c1 c2 = (configuration, c1', c2')
   where configuration = ComponentConfiguration
                            [AnyComponent c1', AnyComponent c2']
                            (usedThreads c1' `max` usedThreads c2')
                            (cost c1' + cost c2')
         c1' = c1 `usingThreads` threads
         c2' = c2 `usingThreads` threads

-- | Function 'optimalTwoParallelConfigurations' configures two components assuming they can be run in parallel,
-- splitting the given thread count between them, and returns the configured components, a 'ComponentConfiguration' that
-- can be used to build a new component from them, and a flag that indicates if they should be run in parallel or
-- sequentially for optimal resource usage.
optimalTwoParallelConfigurations :: Int -> Component c1 -> Component c2
                                 -> (ComponentConfiguration, Component c1, Component c2, Bool)
optimalTwoParallelConfigurations threads c1 c2 = (configuration, c1', c2', parallelize)
   where parallelize = threads > 1 && parallelCost + 1 < sequentialCost
         configuration = ComponentConfiguration
                            [AnyComponent c1', AnyComponent c2']
                            (if parallelize then usedThreads c1' + usedThreads c2' else usedThreads c1' `max` usedThreads c2')
                            (if parallelize then parallelCost + 1 else sequentialCost)
         (c1', c2') = if parallelize then (c1p, c2p) else (c1s, c2s)
         (c1p, c2p, parallelCost) = minimumBy
                                       (\(_, _, cost1) (_, _, cost2)-> compare cost1 cost2)
                                       [let c2threads = threads - c1threads `min` maxUsableThreads c2
                                            c1i = usingThreads c1 c1threads
                                            c2i = usingThreads c2 c2threads
                                        in (c1i, c2i, cost c1i `max` cost c2i)
                                        | c1threads <- [1 .. threads - 1 `min` maxUsableThreads c1]]
         c1s = usingThreads c1 threads
         c2s = usingThreads c2 threads
         sequentialCost = cost c1s + cost c2s

-- | Applies a unary /combinator/ to the component payload. The resulting component has the original one as its
-- 'subComponents', and its 'cost' is the sum of the original component's cost and the /combinator cost/.
lift :: Int {- ^ combinator cost -} -> String {- ^ name -} -> (c1 -> c2) {- ^ combinator -} -> Component c1 -> Component c2
lift wrapperCost name combinator c =
   toComponent name (maxUsableThreads c) $
      \threads-> let c' = usingThreads c threads
                 in (ComponentConfiguration [AnyComponent c'] (usedThreads c') (cost c' + wrapperCost),
                     combinator (with c'))

-- | Combines two components into one, applying /combinator/ to their contents. The 'cost' and 'usingThreads' of the
-- result assume the sequential execution of the argument components.
liftSequentialPair :: String -> (c1 -> c2 -> c3) -> Component c1 -> Component c2 -> Component c3
liftSequentialPair name combinator c1 c2 =
   toComponent name (maxUsableThreads c1 `max` maxUsableThreads c2) $
      \threads-> let (configuration, c1', c2') = optimalTwoSequentialConfigurations threads c1 c2
                 in (configuration, combinator (with c1') (with c2'))

-- | Combines two components into one, applying /combinator/ to their contents. The /combinator/ takes a flag denoting
-- if its arguments should run in parallel. The 'cost' and 'usingThreads' of the result assume the parallel execution of
-- the argument components.
liftParallelPair :: String -> (Bool -> c1 -> c2 -> c3) -> Component c1 -> Component c2 -> Component c3
liftParallelPair name combinator c1 c2 =
   toComponent name (maxUsableThreads c1 + maxUsableThreads c2) $
      \threads-> let (configuration, c1', c2', parallel) = optimalTwoParallelConfigurations threads c1 c2
                 in (configuration, combinator parallel (with c1') (with c2'))

-- | Combines three components into one. The first component runs in parallel with the latter two, which are considered
-- alternative to each other.
parallelRouterAndBranches :: String -> (Bool -> c1 -> c2 -> c3 -> c4) -> Component c1 -> Component c2 -> Component c3
                          -> Component c4
parallelRouterAndBranches name combinator router c1 c2 =
   toComponent name (maxUsableThreads router + maxUsableThreads c1 + maxUsableThreads c2) $
      \threads-> let (cfg, router', c'', parallel) = optimalTwoParallelConfigurations threads router c'
                     (c1'', c2'') = with c''
                     c' = toComponent "branches" (maxUsableThreads c1 `max` maxUsableThreads c2) $
                          \threads-> let (cfg, c1', c2') = optimalTwoAlternatingConfigurations threads c1 c2
                                     in (cfg, (c1', c2'))
                 in (cfg, combinator parallel (with router') (with c1'') (with c2''))

-- | Builds a tree of recursive components. The combinator takes a list of pairs of a boolean flag denoting whether the
-- level should be run in parallel and the value.
recursiveComponentTree :: forall c1 c2. String -> ([(Bool, c1)] -> c2) -> Component c1 -> Component c2
recursiveComponentTree name combinator c =
   toComponent name maxBound $
   \threads-> let (configuration, levels) = optimalRecursion threads
                  optimalRecursion :: Int -> (ComponentConfiguration, [(Bool, c1)])
                  optimalRecursion threads =
                     let (configuration, c', levels', parallel) = optimalTwoParallelConfigurations threads c r
                         r = toComponent name maxBound optimalRecursion
                     in (configuration, (parallel, with c') : with levels')
              in (configuration, combinator levels)
