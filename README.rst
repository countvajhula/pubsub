.. image:: https://github.com/countvajhula/pubsub/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/pubsub/actions

.. image:: https://coveralls.io/repos/github/countvajhula/pubsub/badge.svg?branch=master
    :target: https://coveralls.io/github/countvajhula/pubsub?branch=master

.. image:: https://melpa.org/packages/pubsub-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/pubsub

.. image:: https://stable.melpa.org/packages/pubsub-badge.svg
    :alt: MELPA Stable
    :target: https://stable.melpa.org/#/pubsub

pubsub
======
A basic publish/subscribe system for Emacs.

About the pub/sub pattern
-------------------------

Publish/subscribe is a standard architectural pattern in software development generally, and this package implements a version of it for Emacs. It is useful in cases where different parts of an application, whether in the same module or across modules and packages, need to communicate but without being coupled in any way.

About this Implementation
-------------------------

Topics are keys in a dynamically-bound, toplevel hash table. The value of a topic is a list of subscribers to it. Each subscriber is a function (a "callback") that accepts a single argument.

New notices may be published on any topic, and all subscribers to that topic are called with the notice as the only argument, at the time of publication. Notices are not persisted.

Notices could be anything, i.e., each notice is a value of any type. The pubsub broker simply forwards it to each subscriber.

Where Would You Use This?
-------------------------

The main benefit of the pub/sub model is *decoupling*. It allows one package to announce an event without being tied to the packages that will react to it. This makes code more modular, flexible, and easier to maintain.

Of course, the standard way to do something like this in Emacs is to use *hooks*.

When would you use pubsub over Emacs Hooks?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Emacs's built-in "hooks" are a simple form of pub/sub. However, they are primarily designed to signal that *an event happened*, not to communicate data. As such, pubsub offers several advantages that make it a better choice in certain situations. You should reach for pubsub when:

- You need to pass data: You want to broadcast not just that an event happened, but also include data about the event. pubsub publishing is designed from the ground up to pass a notice to all subscribers.

- You need many "topics": Hooks require creating a new global variable for each topic (e.g., ``prog-mode-hook``). pubsub lets you create topics dynamically from strings or symbols without cluttering the global namespace.

- You need robust error handling: If one subscriber to a standard hook fails with an error, it can stop the entire chain. pubsub isolates subscribers, so a faulty one will be gracefully disabled without affecting the others.

- You need to manage subscribers by name: pubsub allows you to subscribe and unsubscribe functions using a stable name, which is easier and more reliable than trying to manage anonymous lambda functions in a hook.

Example Applications
~~~~~~~~~~~~~~~~~~~~

The `Mantra <https://github.com/countvajhula/mantra>`_ package parses user activity into a high level and precise descriptions of what happened, for instance, recording key sequences or text insertions into the buffer, or window changes, or anything else of interest. It publishes these parsed events on a topic, say, "mantra-keyboard-activity", without needing to know who might be interested in this data. The `Symex <https://github.com/drym-org/symex.el>`_ package subscribes to such parsed keyboard activity for the purposes of allowing users to repeat recent actions in a flexible way. The data could also be used to create a command history, compute statistics on keyboard activity (e.g., "most common words typed" or "most frequent commands used"), or power a tutorial tool that detects inefficient key sequences and suggests better alternatives. None of these tools need to know about each other and do not interfere with one another, and can be added and removed at will, without requiring changes in the others.

We could also imagine a linter for a particular programming language. As it parses the buffer, it could signal errors and warnings on-the-fly by publishing these on topics. There could be one subscriber that marks out each error in the source buffer as it is found, and another that could update the mode line with the number of errors encountered. Since the medium of communication is a standard and public one (i.e., ``pubsub``), we could even subscribe to these notices in custom code to take whatever action we like (e.g., publish the results to a website tracking the build status of your program to share with collaborators), and be confident that it won't interfere with existing operation and any other subscribers.

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
