# Contributing to Nitrogen Web Framework

Note: These guidelines apply to the Nitrogen Web Framework and it's subprojects:

 + [nitrogen](http://github.com/nitrogen/nitrogen)
 + [nitrogen\_core](http://github.com/nitrogen/nitrogen_core)
 + [simple\_bridge](http://github.com/nitrogen/simple_bridge)
 + [nprocreg](http://github.com/nitrogen/nprocreg)
 + [sample\_nitrogen\_plugin](http://github.com/nitrogen/sample_nitrogen_plugin)


## Basic Pull Request Guidelines

Contributions from the community are thoroughly welcome and pull requests are
fabulous! Those who make pull requests are even more fabulous! But there are
some things to consider when you've got the itch to make a pull request.

### Licensing (boring semi-legal stuff first)

By making a pull request to be merged into Nitrogen or one of its related
repositories, you grant to the Nitrogen Project, its maintainers, and its users
an irrevocable and unconditional license to the contributed code, and the
contributed code will be licensed under the [MIT
License](https://github.com/nitrogen/nitrogen/blob/master/MIT-LICENSE).

If a pull request's contents contain any "Viral Open Source Licenses", like the
GPL, or non-commercial-only clauses, we will reject the pull request. We have
commercial organizations that use Nitrogen and we don't want to shackle them
into releasing their source code as well, further we cannot include into
mainline Nitrogen a component that in order to be used by profit-seeking entity
must first purchase a "commercial license" (some javascript libraries function
this way).

In short:
  + **No** GPL, unless dual licensed with the MIT, BSD, or similar free
    licenses.
  + **No** non-commercial licenses or "pay us to use in for profit"-type
    licenses.
  + **Yes** to MIT Licensed free code

**If you really want to use something that is GPL or non-free**, the way to
accomplish this is with the use of [Nitrogen
plugins](http://nitrogenproject.com/doc/plugins.html). This way the entire
plugin can be distributed separately, and if a user wishes to incorporate that
plugin into their application, they can do so by their choice.

### Please try to keep the number of commits down

While we're okay with pull requests being more than one commit, we *prefer*
that pull requests be as minimal as possible (as few commits as possible,
possibly squashed with `git rebase -i`).

### Non-functioning commits should be rebased before being submitted

That said, we will be very picky about pull requests with non-functioning
commits (for example, commits with commit messages as "Work in Progress",
usually mean it doesn't work). Please squash those out with `git rebase`.

### Commit messages

Commit messages should try to be at most 50 characters, but if you go over a
few, it's not a big deal.  Keep the commit messages as concise as possible, and
if you need to add more elaboration, put it on the lines below the commit.
Please try to keep those lines under 80 characters. This can be done quite
trivially in vim with the key combination `gq`.

### Topic Branches

One of the things you'll notice if you do a pull request from your master
branch is that any future commits to that master branch will be automatically
added to the pull request.  *That's a problem*, because then it muddles the
intent of the pull request.  Please make a topic branch (`git checkout -b
my-awesome-fix`) before submitting the pull request, then submit the pull
request from your topic branch.

## Bugfixes

### Have a simple (few-line) fix?

If you have a fix, especially if it's just a simple fix, put in a pull request.
No real need to submit an issue first.

### Have a more complicated fix?

If you have a complicated fix - something that touches lots of files, and may
or may not break API compatibility - go ahead and submit that pull request,
too. It's just going to take longer to pull and it might involve a discussion
of sorts.

### Have something that you think might need fixing, but aren't sure?

If you've encountered a bug, and think you'd like to fix it, but aren't sure if
it's even a bug (hey, it happens, software can be almost as ambiguous as
humans), submit an issue or send a message to the mailing list, or get your
hands dirty and make a fix as a proof-of-concept. Maybe it will be pulled
as-is, and maybe it'll require some tweaking before or after being pulled.

## Feature Requests

### Documentation for new Features

Along with any additions, we really, *really*, **really** would like you to
take the time to put together documentation for the changes. Just think, a
feature is never quite as fresh in your mind as when you first make it. As a
result, it makes sense to document your new baby **right away**, thereby
helping the world to know that your feature is there. So please, if you add a
feature, please add to the pertinent documentation:

  + [Nitrogen Documentation](https://github.com/nitrogen/nitrogen_core/tree/master/doc/org-mode)
    + Uses Emacs Org-mode, but can be edited with any text editor
    + For new elements, actions, validators, or handlers(!), it's probably
      easiest to just copy an existing element, action, etc, than to start from
      scratch.
    + For everything else, finding where to add the documentation is pretty
      straightforward.
    + In the cases of more complex elements, adding a demo to the
      [official Nitrogen site](https://github.com/nitrogen/NitrogenProject.com/tree/master/src/demos)

  + [Simple Bridge Documentation](https://github.com/nitrogen/simple_bridge/blob/master/README.markdown)
    + Just edit the Simple Bridge Readme (linked above)

  + nprocreg doesn't really have any documentation, but if you want to add it, go right ahead!

### Want to add something simple?

Want to add an attribute to an element or add an simple element of your own? Go
ahead and code it up and put in the pull request.

### Want to add something more complicated?

If you have something more mildly complicated, but aren't sure if it's
something that should be added to the `nitrogen_core` repo (like a complex
element, that does lots of fun things, and pulls in some Javascript files as
well), post an issue, or make a post to the mailing list.  We don't want to
fill the global namespace too badly with a pile of elements only a few will
use.

In some scenarios, this would make more sense as a [Nitrogen
Plugin](http://github.com/nitrogen/sample_nitrogen_plugin), rather than as
something core.

### Removing things

Removing things is obviously touchy - that's something that basically
necessitates breaking API.  Post an issue or to the mailing list about it.
That's something we can talk about for a future major version.

### Adding Unit Tests

One of the things we need more of in Nitrogen are unit tests.  We've been
getting by by the seat of our pants, so to speak, in not having any unit tests.
That's obviously a problem. Feel free to help us out there by adding as few or
as many unit tests in some of the core Nitrogen library modules as you'd like.


## Documentation

### Fuond a typo to fix?

Find a typo in our docs? Go ahead and fix that baby (and yes, the typo in the
header here is intentional)

### Want to add missing documentation?

Want to add missing documentation for, say, elements, actions, or validators?
Go ahead and add them and make a pull request. That sounds fantastic.

### Fix wording on docs?

Find some wording that doesn't quite get across what's intended? Go ahead and
revise it to what you think sounds clearer and make a pull request.  At the
very least, it will be considered and will start a conversation about what's
unclear in the documentation.


## Not sure where to start?

Not sure where to start, but want to contribute? Check out the [TODO
List](https://github.com/nitrogen/nitrogen/blob/master/TODO.markdown),
[Documentation TODO
List](https://github.com/nitrogen/nitrogen_core/blob/master/doc/org-mode/README.markdown),
or any outstanding issues on our github repos.

Or contact Jesse Gumm ([@jessegumm](http://twitter.com/jessegumm)), or email
the [mailing list](https://groups.google.com/forum/#!forum/nitrogenweb)
