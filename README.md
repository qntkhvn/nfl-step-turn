## About

Repository for the paper
*Bayesian multilevel step-and-turn models for evaluating player movement in American football*.

A preprint is available at https://arxiv.org/abs/2603.17866.

## Abstract

In sports analytics, player tracking data have driven significant advancements
in the task of player evaluation. We present a novel generative framework for
evaluating the observed frame-by-frame player positioning against a distribution
of hypothetical alternatives. We illustrate our approach by modeling the within-play
movement of an individual ball carrier in the National Football League (NFL).
Specifically, we develop Bayesian multilevel models for frame-level player movement
based on two components: step length (distance between successive locations)
and turn angle (change in direction between successive steps). Using the
step-and-turn models, we perform posterior predictive simulation to generate
hypothetical ball carrier steps at each frame during a play. This enables
comparison of the observed player movement with a distribution of simulated
alternatives using common valuation measures in American football. We apply
our framework to tracking data from the first nine weeks of the 2022 NFL season
and derive novel player performance metrics based on hypothetical evaluation.