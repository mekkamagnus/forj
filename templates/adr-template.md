<!--
How to use this template (atomic ADR guidance):
1. Copy this file to [`docs/adr/000X-your-title.md`](docs/adr/0001-your-title.md:1) — replace 000X with the next sequential ID.
2. Keep each ADR atomic: document a single, self-contained decision. Keep it concise (1–3 pages).
3. Fill required metadata (Title, Status, Date, Authors), then add minimal Context, Decision, Consequences, Alternatives, and Rationale.
4. Do not embed long design documents — link to them instead (use References).
5. When superseding a decision, create a new ADR and set Status: superseded; do not overwrite historical ADR text.
6. Commit ADRs alongside the related code/PR and reference the issue/PR for traceability.
-->
<!-- ADR Template -->

# ADR Template — 000X Short Title

<!-- Metadata -->

**Title:** 000X — Short human-friendly title  
**Status:** proposed | accepted | deprecated | superseded  
**Date:** 2025-08-31  
**Authors:** Your Name <email@example.com>

## Context

- Short background and constraints that make this decision necessary.

## Decision

- A clear, one-line statement of the chosen option.

## Consequences

- Positive consequences
- Negative consequences

## Alternatives Considered

- **Option A** — brief pros/cons
- **Option B** — brief pros/cons
- **Option C** — brief pros/cons

## Rationale

- Why the chosen option was selected, linked to constraints and tradeoffs.

## Related ADRs

- 0001 — Some previous decision

## References

- Issue #123 — link or description
- Benchmark report — link

## Implementation Notes

- Short list of steps needed to enact the decision (deploy, migrate, etc.).

## Rollback / Migration Plan

- Steps to revert or migrate if the decision is later reversed.

---

## How to use this template

1. Copy this file to `docs/adr/000X-your-title.md` (replace 000X with next sequential number).
2. Fill in each section with concise, factual information.
3. Commit and link the ADR to the relevant issue or PR.

Notes:

- Keep ADRs short (1–3 pages).
- Update the Status when the ADR is superseded; don't rewrite history.
- Use plain Markdown so ADRs are easy to diff and review.
