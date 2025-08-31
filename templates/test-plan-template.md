# Test Plan Template

## Title

Test Plan for: [Feature / Module Name]

## Prepared By

- Author:
- Date:

## Objective

Concise statement of what this test plan validates.

## Scope

- In scope
- Out of scope

## Acceptance Criteria

List concrete, testable acceptance criteria that map to README examples and specs.

## Test Items

- Unit tests
- Integration tests
- End-to-end tests
- Performance tests
- Security tests

## Test Approach

Describe the testing strategy for each level:

- Unit: ERT unit tests (fast, isolated)
- Integration: invoke package-level flows, use temp buffers and mocks
- E2E: end-to-end scenarios using emacs -batch where applicable

## Traceability Matrix

| Requirement / Example              |                        Test(s) | Status  |
| ---------------------------------- | -----------------------------: | :-----: |
| README example: forj-prompt sample | test/forj-conversation-test.el | Planned |

## Test Data & Environments

- Local dev: Emacs >= 27.1
- CI: Headless emacs in GitHub Actions
- Test fixtures: `test/test-cases.el`

## Entry Criteria

- README examples drafted
- Spec or PR describing behavior
- Development branch with initial implementation

## Exit Criteria

- All blocking tests passing
- No critical or high severity regressions open
- Test coverage thresholds met (project target)

## Schedule & Responsibilities

- Test author:
- Reviewers:
- Dates / milestones

## Risks & Mitigations

- Risk: External API flakiness
  - Mitigation: Mock API responses and add retry logic in tests

## Reporting & Metrics

- Test run frequency
- Test pass/fail reporting (CI artifact links)
- Test duration metrics

## Execution Instructions

- Run unit tests:

```bash
make test
# or
emacs -batch -l ert -l test/forj-test.el -f ert-run-tests-batch-and-exit
```

## Notes

- Follow RDD: convert README examples into ERT tests as the primary acceptance tests.
