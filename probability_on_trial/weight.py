import warnings
from typing import Dict, List

import pyro.distributions as dist
import torch


def normalize_sample(sample: torch.Tensor, k: int = 1000) -> torch.Tensor:
    """
    `sample` is supposed to be a sample of values, returns probabilites across k bins.
    """

    hist = torch.histc(sample, bins=k)
    return hist / hist.sum()


def weight(posterior: torch.Tensor, base=2.0) -> torch.Tensor:
    """
    Calculates the weight of the posterior distribution.
    The weight is defined as 1 - H(posterior) / H(uniform)
    where H is the entropy of the distribution.
        param: posterior: torch.Tensor - tensor of probabilities (not samples).
    """

    if not isinstance(base, torch.Tensor):  # Check if base is not a tensor
        base = torch.tensor(base)

    initial_length = posterior.shape[-1]
    posterior = posterior[~torch.isnan(posterior)]
    removed_length = initial_length - posterior.shape[-1]

    if initial_length != posterior.shape[-1]:  # raise warning
        warnings.warn(f"Removed {removed_length} nan values from posterior")

    if posterior.numel() == 0:
        return torch.tensor(float("nan"))

    grid_length = posterior.shape[-1]
    x = torch.linspace(0, 1, grid_length)

    uniform = dist.Beta(1, 1).log_prob(x).exp()
    uniform = uniform / uniform.sum()

    assert torch.allclose(
        uniform.sum(), torch.tensor(1.0)
    ), f"Sum of uniform distribution is {uniform.sum()}"
    assert torch.allclose(
        posterior.sum(), torch.tensor(1.0)
    ), f"Sum of posterior distribution is {posterior.sum()}"

    entropy_uniform = -torch.sum(uniform * torch.log(uniform) / torch.log(base))

    entropy_posterior = -torch.sum(posterior * torch.log(posterior) / torch.log(base))

    return 1 - entropy_posterior / entropy_uniform


def expected_weight(
    probs_of_evidence: torch.Tensor,
    outcome_prior: torch.Tensor,
    posteriors: List[torch.Tensor],
    base=2,
) -> Dict[str, torch.Tensor]:
    """
    Calculate the expected weight change given a prior distribution,
    posterior distributions, and the probabilities of evidence.

    Args:
    probs_of_evidence (torch.Tensor): A tensor of probabilities representing the likelihood of the evidence.
    outcome_prior (torch.Tensor): A tensor representing the prior distribution of outcomes.
    posteriors (List[torch.Tensor]): A list of tensors representing the posterior distributions.
    base (int): The base for calculating the weight (default is 2).

    """

    weight_prior = weight(outcome_prior, base=base)

    posterior_weights = torch.tensor(
        [weight(posterior, base=base) for posterior in posteriors]
    )

    print(posterior_weights)
    weight_changes = posterior_weights - weight_prior

    weighted_weight_changes = weight_changes * probs_of_evidence

    assert probs_of_evidence.shape == weighted_weight_changes.shape, "shape mismatch"

    expected_weight = weighted_weight_changes.sum()

    return {
        "expected_weight": expected_weight,
        "weight_prior": weight_prior,
        "posterior_weights": posterior_weights,
        "weight_changes": weight_changes,
        "weighted_weight_changes": weighted_weight_changes,
    }
