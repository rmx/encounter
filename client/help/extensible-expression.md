---
title: 'Extensible Expression'
tags: [storage]
---

Extensible expressions are a sumtype of Expression and something else. They
are used in the storage layer to allow us to extend the types in the future.

For example, the [Scoring Function][article:scoring-function] is currently
only available as a plain [Expression][article:expressions], but in the future
we may add a more structured representation to make it easier for authors to
edit it.
