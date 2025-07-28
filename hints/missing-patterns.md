# Hints for Missing Patterns

Gren checks to make sure that all possible inputs to a function or `when` are handled. This gives us the guarantee that no Gren code is ever going to crash because data had an unexpected shape.

There are a couple techniques for making this work for you in every scenario.

## The danger of wildcard patterns

A common scenario is that you want to add a tag to a custom type that is used in a bunch of places. For example, maybe you are working different variations of users in a chat room:

```gren
type User
  = Regular String Int
  | Anonymous

toName : User -> String
toName user =
  when user is
    Regular name _ ->
      name

    _ ->
      "anonymous"
```

Notice the wildcard pattern in `toName`. This will hurt us! Say we add a `Visitor String` variant to `User` at some point. Now we have a bug that visitor names are reported as `"anonymous"`, and the compiler cannot help us!

So instead, it is better to explicitly list all possible variants, like this:

```gren
type User
  = Regular String Int
  | Visitor String
  | Anonymous

toName : User -> String
toName user =
  when user is
    Regular name _ ->
      name

    Anonymous ->
      "anonymous"
```

Now the compiler will say "hey, what should `toName` do when it sees a `Visitor`?" This is a tiny bit of extra work, but it is very worth it!

## I want to go fast!

Imagine that the `User` type appears in 20 or 30 functions across your project. When we add a `Visitor` variant, the compiler points out all the places that need to be updated. That is very convenient, but in a big project, maybe you want to get through it extra quickly.

In that case, it can be helpful to use [`Debug.todo`](https://packages.gren-lang.org/package/gren-lang/core/latest/module/Debug#todo) to leave some code incomplete:

```gren
type User
  = Regular String Int
  | Visitor String
  | Anonymous

toName : User -> String
toName user =
  when user is
    Regular name _ ->
      name

    Visitor _ ->
      Debug.todo "give the visitor name"

    Anonymous ->
      "anonymous"

-- and maybe a bunch of other things
```

In this case it is easier to just write the implementation, but the point is that on more complex functions, you can put things off a bit.

The Gren compiler is actually aware of `Debug.todo` so when it sees it in a `when` like this, it will crash with a bunch of helpful information. It will tell you:

1. The name of the module that contains the code.
2. The line numbers of the `when` containing the TODO.
3. The particular value that led to this TODO.

From that information you have a pretty good idea of what went wrong and can go fix it.

I tend to use `Debug.todo` as the message when my goal is to go quick because it makes it easy to go and find all remaining todos in my code before a release.

## A list that definitely is not empty

This can come up from time to time, but Gren **will not** let you write code like this:

```gren
last : List a -> a
last list =
  when list is
    [x] ->
        x

    _ :: rest ->
        last rest
```

This is no good. It does not handle the empty list. There are two ways to handle this. One is to make the function return a `Maybe` like this:

```gren
last : List a -> Maybe a
last list =
  when list is
    [] ->
        Nothing

    [x] ->
        Just x

    _ :: rest ->
        last rest
```

This is nice because it lets users know that there might be a failure, so they can recover from it however they want.

The other option is to “unroll the list” one level to ensure that no one can ever provide an empty list in the first place:

```gren
last : a -> List a -> a
last first rest =
  when rest is
    [] ->
      first

    newFirst :: newRest ->
      last newFirst newRest
```

By demanding the first element of the list as an argument, it becomes impossible to call this function if you have an empty list!

This “unroll the list” trick is quite useful. I recommend using it directly, not through some external library. It is nothing special. Just a useful idea!
