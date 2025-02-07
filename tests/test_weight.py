import torch

from probability_on_trial.weight import weight

probs_of_evidence = torch.tensor([0.1, 0.5, 0.4])
outcome_prior = torch.tensor([0.25, 0.25, 0.25, 0.25])
posteriors_list = [
    torch.tensor([0.6, 0.2, 0.1, 0.1]),
    torch.tensor([0.3, 0.4, 0.1, 0.2]),
    torch.tensor([0.25, 0.35, 0.2, 0.2]),
]

posteriors = torch.stack(posteriors_list, dim=-2)
# there are three distributions with four states


def test_weight():
    # calculate on a single distribtion:
    single_weight = weight(posteriors[0])
    assert torch.allclose(single_weight, torch.tensor(0.2145), atol=0.01)

    # calculate on a batch of distributions:
    batched_weights = weight(posteriors)
    assert torch.allclose(
        batched_weights, torch.tensor([0.2145, 0.0768, 0.0206]), atol=0.01
    )


#  The expected value of the evidence weight change is computed as:

#     ```math
#     \mathbb{E}[E, PRIOR] = \sum_{i=1}^{k} P(S_i \mid PRIOR) \times \text{VAL}[E_i, PRIOR]
#     ```

#     The expected weight change due to evidence is given by:

#     ```math
#     E_{\text{Vchange}}[E, PRIOR] = \sum_{i=1}^{k} P(E_i \mid PRIOR) \times
#     \left( \text{VAL}(\text{POSTERIOR}_{E_i}) - \text{VAL}(\text{PRIOR}) \right)
#     ```
