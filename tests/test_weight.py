import torch
from probability_on_trial.weight import weight, expected_weight


probs_of_evidence = torch.tensor([0.1, 0.5, 0.4])
outcome_prior = torch.tensor([0.25, 0.25, 0.25, 0.25])
posteriors = [
    torch.tensor([0.6, 0.2, 0.1, 0.1]),
    torch.tensor([0.3, 0.4, 0.1, 0.2]),
    torch.tensor([0.25, 0.35, 0.2, 0.2]),
]

posteriors = torch.stack(posteriors, dim=-2)
# there are three distributions with four states

def test_weight():
    #calculate on a single distribtion:
    single_weight = weight(posteriors[0])
    assert torch.allclose(single_weight,
        torch.tensor(0.2145), atol = .01)

    # calculate on a batch of distributions:
    batched_weights = weight(posteriors)
    assert torch.allclose(batched_weights,
        torch.tensor([0.2145, 0.0768, 0.0206]), atol = .01)

