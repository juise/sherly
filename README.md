sherly
======

Erlang web shell, inspired by [erlwsh](https://github.com/killme2008/erlwsh)


Building
--------

The building sherly should be very simple as:

``` sh
$ git clone git@github.com:juise/sherly.git
$ cd sherly
$ make rel
```

Starting
--------

Once you have successfully built sherly, you can start the server with the following commands:

```
$ cd rel/sherly
$ bin/sherly start
```

Or you can start the server in the development mode with the following command:

```
$ make run
```

It's very simple, isn't it, huh?

Working with web shell
----------------------

Now, when you successfully started server, you can type in your browser [http://localhost:8080/](http://localhost:8080/) and start working in the web shell.
