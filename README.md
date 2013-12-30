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

``` sh
$ cd rel/sherly
$ bin/sherly start
```

Or you can start the server in the development mode with the following command:

``` sh
$ make run
```

It's very simple, isn't it, huh?

Working with web shell
----------------------

Now, when you successfully started server, you can type in your browser [http://localhost:8080/](http://localhost:8080/) and start working in the web shell.

Sherly 'n' Nginx
----------------

Once you have successfully build sherly, copy it to www directory (/tmp by example), and setting up to use your public domain name (example.com:80 by example)

``` sh
$ cp -rvf rel/sherly /tmp/sherly
$ sed -i.bak "s/localhost:8080/example.com:80/g" /tmp/sherly/lib/sherly-1.0.0/priv/index.html
```

Teach nginx to work with sherly through websockets:

```
http {
...
   map $http_upgrade $connection_upgrade {
        default upgrade;
        ''      close;
    }
...
   server {
        listen      80;
        server_name example.com;
...
        location /sherly {
            alias   /tmp/sherly/lib/sherly-1.0.0/priv/;
            index   index.html;
        }
        
        location /static/ {
            root    /tmp/sherly/lib/sherly-1.0.0/priv;
        }     
        
        location /websocket {
            proxy_pass          http://localhost:8080;
            proxy_http_version  1.1;

            proxy_set_header    Upgrade $http_upgrade;
            proxy_set_header    Connection $connection_upgrade;
        }
...
   }
}
```

And now you can start the server with the following command:

``` sh
$ /tmp/sherly/bin/sherly start
```
