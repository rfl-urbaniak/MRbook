import warnings
from typing import Dict, Optional

import pyro.distributions as dist
import torch


def normalize_sample(sample: torch.Tensor, k: int = 1000) -> torch.Tensor:
    """
    `sample` is supposed to be a sample of values, returns probabilites across k bins.
    """

    hist = torch.histc(sample, bins=k)
    return hist / hist.sum()


def weight(
    posterior: torch.Tensor, prior: Optional[torch.Tensor] = None, base=2.0,
    epsilon = 1e-10
) -> torch.Tensor:
    """
    Calculates the weight of the posterior distribution.
    The weight is defined as 1 - H(posterior) / H(prior)
    where H is the entropy of the distribution.
        param: posterior: torch.Tensor - tensor of probabilities (not samples).
        Probabilities need to sum to 1 in dimension -1, can be batched in dimension -2.
        See test_weight.py for examples.
    """

    if not isinstance(base, torch.Tensor):  # Check if base is not a tensor
        base = torch.tensor(base)

    # else:
    #     prior = prior.broadcast_to(posterior.shape)
    if prior is not None and prior.dim() == 0:
        warnings.warn("prior is a scalar, assuming it is a probability of success")
        prior_no = 1 - prior
        prior_distro = torch.stack([prior_no, prior], dim=-1)
        prior = prior_distro.broadcast_to(posterior.shape)

    assert torch.allclose(
        posterior.sum(dim=-1, keepdim=True), torch.tensor(1.0)
    ), f"Sum of posterior distribution is {posterior.sum()}"

    if posterior.isnan().sum() > 0:
        raise ValueError("Posterior contains nan values!")

    if posterior.numel() == 0:
        return torch.tensor(float("nan"))

    if prior is None:
        grid_length = posterior.shape[-1]
        x = torch.linspace(0, 1, grid_length)
        x = torch.broadcast_to(x, posterior.shape)

        prior = dist.Beta(1, 1).log_prob(x).exp()
        prior = prior / prior.sum(dim=-1, keepdim=True)

        assert torch.allclose(prior.sum(dim=-1), torch.tensor(1.0))

    entropy_prior = -torch.sum(prior * torch.log(prior) / torch.log(base), dim=-1)

    entropy_posterior = -torch.sum(
        posterior * torch.log(posterior+epsilon) / torch.log(base), dim=-1
    )

    return 1 - entropy_posterior / entropy_prior


def expected_weight(
    probs_of_evidence: torch.Tensor,
    outcome_prior: torch.Tensor,
    posteriors: torch.Tensor,
    base=2,
) -> Dict[str, torch.Tensor]:
    """
    Compute the expected weight change given a prior distribution, posterior distributions,
    and the probabilities of evidence.

    The function evaluates how much the weight of evidence changes based on different
    posterior distributions, adjusting for the probabilities of the evidence.

    Args:
        probs_of_evidence (torch.Tensor): A tensor representing the likelihood of each evidence scenario.
        outcome_prior (torch.Tensor): A tensor representing the prior distribution over outcomes.
        posteriors (List[torch.Tensor]): A list of tensors, each representing a posterior distribution.
        base (int, optional): The logarithmic base used in weight calculations (default: 2).

    Returns:
        Dict[str, torch.Tensor]: A dictionary containing the following elements:

        - **expected_weight** (`torch.Tensor`): The overall expected change in weight across possible
          evidence values, incorporating both prior information and posterior adjustments.

        - **weight_prior** (`torch.Tensor`): The weight of the prior distribution before observing any evidence.

        - **posterior_weights** (`torch.Tensor`): The computed weights of the posterior distributions.
          A posterior weight close to 1 indicates more certainty (lower entropy), whereas a value close
          to 0 suggests higher uncertainty.

        - **weight_changes** (`torch.Tensor`): The differences between posterior weights and the prior weight,
          quantifying how much each posterior deviates from the prior.

        - **weighted_weight_changes** (`torch.Tensor`): The `weight_changes` values scaled by their respective
          probabilities (`probs_of_evidence`), representing each piece of evidence's contribution to the
          overall expected weight change.

    Mathematical Formulation:


    """

    weight_prior = weight(outcome_prior, base=base)

    posterior_weights = weight(posteriors, base=base)

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
