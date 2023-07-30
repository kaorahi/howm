=begin

= howm tutorial

== First day

=== Write a note.

* Type Ctrl-C , , to open the menu.
    [New] Search([String] [Regexp]) [Dup] [<Title] [<Name] [Date>] [Key>] [Update]
    [All] [Recent] [Schedule] [Todo] [Update Menu]
* Confirm the cursor is on [New], and type [return] there to open new note.
    = 
    [2002-09-16 20:08] >>> /home/hira/elisp/howm/howm-mode.el
* Write anything.
    = First note
    [2002-09-16 20:08] >>> /home/hira/elisp/howm/howm-mode.el
    foo
    bar
    baz


== Second day

=== Write today's note.

* Type Ctrl-C , , and type [return] on [New]. Then, write freely.

=== Read notes.

* Type Ctrl-C , , to open the menu.
    [New] Search([String] [Regexp]) [Dup] [<Title] [<Name] [Date>] [Key>] [Update]
    [All] [Recent] [Schedule] [Todo] [Update Menu]
* Move the cursor to [All] and type [return] there to browse all notes.
* Move the cursor up and down to view each note.
* Type [space] and [backspace] to scroll the note.
* Type [return] to open the note.

=== Concatenate notes.

* Type Ctrl-C , , to open the menu.
* Move the cursor to [All] and type [return].
* Type @ to concatenate all notes.
* Type [space] and [backspace] to scroll the notes.
* Type @ to toggle summary-view and concatenated-view.
* Type [return] to open the note.

=== Search a word in notes.

* Type Ctrl-C , , to open the menu.
* Move the cursor to [String] and type [return] there to search for a string.
    Keyword:
* Input a string.
    Keyword: foo
* Type [return] to browse matched notes.

=== Search a phrase in notes.

* Type Ctrl-C , , to open the menu.
* Move the cursor to [String] and type [return].
* Input a string. Type Ctrl-q and [space] to input a whitespace.
    Keyword: hoge fuga
* Type [return] to browse matched notes.


== Third day

=== Try goto link.

* Ctrl-C , , and [return] on [New] to open new note.
* Write '>>> foo'.
    = 
    [2002-09-17 20:15]
    >>> foo
* A line is drawn under it.
  Move the cursor on it and type [return] to search for 'foo' in all notes.

=== Try come-from link.

* Write '<<< hoge' in today's note.
    = 
    [2002-09-17 20:15]
    >>> foo
    <<< hoge
* Move the cursor on '<<<' and type [return] to search for 'hoge' in all notes.
* Select the first day's note and type [return] to open it.
* A line is drawn under 'hoge'.
  Move the cursor on it and type [return] to search for 'hoge' in all notes.
* Remember that '<<< hoge' is always shown first when you search for 'hoge'.
  Others are sorted by their modification time.

=== Search come-from keyword with completion.

* Ctrl-C , , and [return] on [String] to search for a keyword.
    Keyword:
* Type 'ho' and type [space] to complete come-from keyword 'hoge'.
    Keyword: hoge
* Type [return] to search for 'hoge'.


== Fourth day

=== Sort notes.

* Ctrl-C , , and [return] on [All] to browse all notes.
* Type S to sort notes.
    Sort by:
* Type [space] to show available criteria.
* Type 'da' and type [space] to complete 'date'.
    Sort by: date
* Type [return] to execute it.

=== Filter notes.

* Ctrl-C , , and [return] on [All] to browse all notes.
* Type f to filter notes.
    Filter by:
* Type [space] to show available criteria.
* Type 'co' and type [space] to complete 'contents'.
    Filter by: contents
* Type [return] and input 'foo'.
    Search in result (grep): foo
* Type [return] to execute it.

=== Read help.

* Ctrl-C , , and [return] on [All] to browse all notes.
* Type ? and read the help message.

=== Use shortcut.

* Type Ctrl-C , c to create new note.


== Fifth day

=== Input todo.

* Type Ctrl-C , c to create new note.
* Write as follows.
    [2003-09-10]- foo
    [2003-09-10]+ bar
    [2003-09-10]! baz
    [2003-09-10]~ qux

=== Browse todo.

* Ctrl-C , , and [return] on [Todo] to browse all todo.
* Move cursor and type [return] to open the note.
* Type Ctrl-C , , to show the head of todo under the menu.
* Items are sorted by a magic criterion.
    - sink after the date (reminder)
    + float after the date (todo)
    ! float until the date (deadline)
    ~ sink and float periodically after the date (defer)

=== Finish todo.

* Ctrl-C , , and [return] on [Todo] to browse all todo.
* Move cursor and type [return] to open the note.
    [2003-09-10]- foo
* Move cursor on - and type [return] to show todo-menu.
    RET (done), x (cancel), symbol (type), num(laziness): 
* Type [return] to finish this todo.
    [2003-09-10]. [2003-09-10]:- foo

=== Input schedule.

* Type Ctrl-C , c to create new note.
* Write as follows.
    [2003-09-10]@ hoge

=== Browse schedule.

* Ctrl-C , , and [return] on [Schedule] to browse all schedule.
* Type [space] and [backspace] to scroll the list.
* Type . to jump to today.
* Type Ctrl-C , , to show the head of schedule under the menu.
* Type [return] on > at the beginning of line to open the corresponding note.

=== Input recurring tasks.

* move cursor to '2003-09-10' on the below form in the opened note.
    [2003-09-10]@ hoge
* Type [return] to show date-menu.
    [Wed] RET(goto), +num(shift), yymmdd(set), ~yymmdd(repeat), .(today): 
* Input ~031231 [return] and then type m [return] to the below question.
    Every? [RET(all), num(days) w(week), m(month), y(year)] 
* And monthly tasks are added automatically until 2003-12-31.
    [2003-09-10]@ hoge
    [2003-10-10]@ hoge
    [2003-11-10]@ hoge
    [2003-12-10]@ hoge
* You can use '~1231' instead of '~031231' to input same-year items.


== Sixth day

=== Try action-lock { }.

* Type Ctrl-C , c to create new note.
* Write { }.
    { }
* Move cursor on { } and type [return].
    {*}
* Type [return] again.
    {-}

=== Try action-lock {_}.

* Type Ctrl-C , c to create new note.
* Write {_}.
    {_}
* Move cursor on {_} and type [return].
    [2003-09-10 04:12]

=== Try aliases.

* Write '<<< hoge <<< fuga <<< piyo' in a note.
    = 
    [2002-09-17 20:15]
    <<< hoge <<< fuga <<< piyo
* Write 'hoge' in another note.
* Write 'fuga' in yet another note.
* Put cursor on 'fuga' and type [return].
  You will see all 'hoge', 'fuga', and 'piyo' are found together.


== Seventh day

=== Try customization.

* Type Ctrl-C , , to show the menu.
* Move cursor on [Preference] and type [return].
* Watch customization items and modify them if you like.

=== Try editing menu.

* Type Ctrl-C , , to show the menu.
* Move cursor on [Edit Menu] and type [return].
* Delete the line '%random', and type Ctrl-C , , to observe its effect.
* Again try [Edit Menu] and write '%random' to restore it.
* Type Ctrl-C , , and move cursor to '%Editing Menu%' at the bottom.
* Type [return] and read documentation of menu syntax.

=end
