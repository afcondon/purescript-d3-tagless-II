# Markov Chain Sankey Ordering - Archived Implementation

## Overview

This directory contains an incomplete implementation of the Markov Chain barycentre ordering algorithm for Sankey diagrams, based on the paper "Crossing Reduction of Sankey Diagram with Barycentre Ordering via Markov Chain" by Li et al. (arXiv:1912.05339).

The implementation was archived on 2024-12-09 because it did not produce better results than the D3-style relaxation heuristic.

## Files

- `MarkovChain.purs` - Core algorithm: eigenvector computation, transition matrices, weighted crossing calculation
- `MarkovChainIntegration.purs` - Integration layer converting SankeyNode/SankeyLink to the algorithm's layer-based format
- `MarkovChainWithSteps.purs` - Step-by-step version for debugging visualization
- `PartitionRefinement.purs` - Stage 2 of the algorithm: iterative block-based refinement

## What Was Implemented

1. **Eigenvector utilities** - Power iteration, deflation for second eigenvector
2. **Transition matrices** - Left (L) and right (R) transition matrices with random perturbation (alpha parameter)
3. **Weighted crossing calculation** - K-bar metric for evaluating ordering quality
4. **Partition refinement** - Stage 2 iterative refinement with block-based edge positioning
5. **Best-in-N trials** - Multiple random trials to find better orderings

## Issues Encountered

### 1. Matrix Dimension Mismatch (Critical)
The paper's algorithm is designed for **two-layer bipartite graphs**. For multi-layer Sankey diagrams, the composite transition matrix `T = R * L` requires multiplying matrices of incompatible dimensions:
- Left matrices have dimensions (layer i+1 nodes) x (layer i nodes)
- Right matrices have dimensions (layer i nodes) x (layer i+1 nodes)
- Multiplying the chain of all left matrices together produces a 0x0 matrix because adjacent matrices don't have compatible inner dimensions

### 2. Layer 0 Initialization
The eigenvector method computes positions for layer 1, then propagates to other layers. Layer 0 positions were initially just sequential indices, leading to trivial orderings (nearly reversed sequential order).

### 3. NaN Propagation Bug (Fixed)
When `iterations = 0`, the relaxation algorithm produced NaN values due to division by zero in beta calculation. This was fixed in the main Compute.purs by adding a guard: `if iterations <= 0 then nodes else ...`

## Attempted Solutions

1. **Barycenter heuristic fallback** - Replaced spectral method with classical barycenter averaging (forward/backward passes). This works but produces orderings very similar to the original node order.

2. **Random initialization** - Could help break symmetry but wasn't fully implemented.

## Positive Outcomes

While debugging the Markov chain implementation, several bugs were fixed in the main PureScript Sankey algorithm:

1. **NaN bug fix** - Added guard for `iterations <= 0` in relaxation
2. **Node processing order** - Added sorting by y0 before processing nodes in relaxLayer
3. **Debug infrastructure** - Added extensive logging for step-by-step visualization comparison

## Future Work

To make this algorithm work properly:

1. **Per-layer-pair approach** - Apply the spectral method to each pair of adjacent layers independently, rather than trying to build a single composite matrix for all layers.

2. **Iterative global optimization** - Run multiple sweeps (left-to-right, right-to-left) and let the orderings converge.

3. **Proper random initialization** - Use random initial positions for all layers, not sequential indices.

4. **Hybrid approach** - Use the spectral method for initial ordering, then apply D3-style relaxation for fine-tuning.

## References

- Li et al., "Crossing Reduction of Sankey Diagram with Barycentre Ordering via Markov Chain", arXiv:1912.05339, 2019
- D3-sankey implementation: https://github.com/d3/d3-sankey
