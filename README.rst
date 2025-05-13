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

Topics are keys in a dynamically-bound, toplevel hash table. The value of a topic is a list of subscribers to it. Each subscriber is a ("callback") function that accepts a single argument.

New notices may be published on any topic, and all subscribers to that topic are called with the notice as the only argument.

Notices could be anything, i.e., each notice is a value of any type. The pubsub system simply forwards it to each subscriber.

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
