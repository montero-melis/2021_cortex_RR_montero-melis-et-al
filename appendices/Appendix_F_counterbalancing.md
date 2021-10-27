Counterbalancing of conditions and items across participants
============================================================

An item consists of a quadruple of words, i.e. a set of 4 words of the same 
type that participants see in a trial. Since there are 52 arm and 52 leg words,
we can form 26 unique items (13 arm items and 13 leg items).

We create 3 sets of items, i.e. 3 different groupings of the same 104 target
words into items (quadruples). We call each of these sets an item List 
(Lists 1, 2 and 3). Each participant will see all three lists, one per block.
So there are 6 different permutations (123, 132, 213, ...).

At the same time, participants carry out the task in three conditions (arm,
leg, control), so there are 6 permutations of conditions as well. All in all,
there are 36 unique ordered combinations of condition-list assignments.

Our counterbalancing scheme makes sure that:

a) Each batch of 6 participants gets assigned all possible conditions (because
   counterbalancing conditions is the most important);
b) That all lists are seen equally often across each batch of 6 participants.
c) That each 36 participants we have a full cycle of conditions-to-lists
	 assignments where both are completely balanced.

This is done in the following script:
https://github.com/montero-melis/2018_replication_sheb-pulv2013/blob/master/1908_sp13_replic_swe/psychopy_exp/stimuli/create_random_condition-list-assignments.R
