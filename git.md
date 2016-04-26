# HPC Course

Session 2

S.Mohammad Hoseini-Rad

[smhr.github.io](http://smhr.github.io/)

## Controlling version

### Git

* Git is a distributed version control system.
* When you clone a repository you get all the history too.
* All stored in .git subdirectory of top directory.

--

#### Main commands: ####

--
* `git commit` commits to your clone’s .git directory.

--
* `git push` sends your recent changesets to another clone
by default: the one you cloned from (e.g. bitbucket).

--
* `git fetch` pulls changesets from another clone
by default: the one you cloned from (e.g. bitbucket).

--
* `git merge` applies changesets to your working copy

### Advantages of distributed model

* You can commit changes, revert to earlier versions,
examine history, etc. without being connected to server.

--
* Also without affecting anyone else’s version if you’re
working collaboratively.

--
* No problem if server dies, every clone has full history.

--
* For collaboration will still need to push or fetch changes
eventually and **git merge** may become more complicated.

### Some useful tools

* `sudo apt-get gitk` (revision tree visualizer)
* `sudo apt-get meld` (graphical tool to diff and merge files)

### Example

```bash
git clone git@github.com:smhr/SC_class.git
```
--

Do someworks, then

--

```bash
git add -A
git commit -m "1st commit"
git push origin master
```
