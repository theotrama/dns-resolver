# Resolving juliusbaer.com with our own DNS resolver

## Introduction

A lot of time we take for granted what happens behind the scenes. Issuing an HTTP request from a browser, well we just
use axios. Running a web server let’s use Spring Boot. Installing a reverse proxy in front of our application, nginx,
traefik, whatever you like. These tools are incredibly powerful and without them we wouldn’t be able to build the
complex systems that we work with today. We are standing on the shoulders of giants. From time to time though we need to
peek behind the curtain and understand what is happening behind these abstractions. May it be a nasty production
incident that we do not
understand or applications that have high performance requirements that we need to tweak for. At this point in
time we start to dig deeper and unravel one more layer of the onion. We go down the rabbit hole. Today I want to dig
deeper and look into a protocol that is empowering the internet. The domain name system, short DNS.

## The protocol

The domain name
system was invented to make the internet more user-friendly. With TCP/IP you of course could use IP addresses to connect
to a server but who can remember that for example juliusbaer.com has the IP of 188.92.48.169? Here DNS comes into play
as it allows us to resolve a server name to an IP address. The way this works is that you have 13 logical root DNS
servers. You can find them here https://www.iana.org/domains/root/servers. These are not physical servers so there can
be much more than 13 physical servers providing root level DNS resolution. Actually there are currently about 1916 root DNS server instances
https://root-servers.org/. These 1916 share the 13 root IP addresses by using ANYCAST. If you now want to resolve juliusbaer.com to
an IP address you can go to any of the 13 DNS root servers and ask: “Hey do you know the IP address of juliusbaer.com?”.
Probably it will not know the address immediately, but it might know which other name server could know about it. It
thus
returns a list of authority records that point to name servers that might know more about juliusbaer.com. If you are
even luckier you get a list of additional records that provide directly an IP address for these authoritative name
servers. So the search goes on. We ask the next name server if it knows the IP address of juliusbaer.com. Again it
doesn’t know, but it knows who might. We continue our query until we get a list of answer records and with that we
finally have resolved juliusbaer.com to 188.92.48.169.

Now, this might seem simple but let’s dive one level deeper and look into the protocol which you can find
here https://datatracker.ietf.org/doc/html/rfc1035. How can we query one
of those root servers for a domain name and how will it answer? Each DNS request and response consists of the following
parts: Header, Question, Answer, Authority, Additional. The header section is the only field with a fixed length. The
remaining sections can grow dynamically. When we send a DNS request we send only the header and the query section. When
we receive a DNS response it can happen that we get for example a header, a query section (repeating the query we
issued), an authority section, and an additional section. But now let's have a look at the full protocol definition
which
you can see in the figure below.

<figure id="figure-1">
  <img src="img/protocol.svg" alt="DNS protocol">
  <figcaption>Figure 1: The DNS protocol</figcaption>
</figure>

So the first query for juliusbaer.com to one of the root DNS servers would look like this.

<figure>
  <img src="img/juliusbaer_dns_request.svg" alt="DNS protocol">
  <figcaption>Figure 2: A DNS request for juliusbaer.com</figcaption>
</figure>

For that we would get the following response.

<figure>
  <img src="img/juliusbaer_dns_response.svg" alt="DNS protocol">
  <figcaption>Figure 3: A DNS response for juliusbaer.com</figcaption>
</figure>

# The process

Now, with the basics of the protocol in our minds let us go through the process of resolving the IP of juliusbaer.com.
We create a DNS request by setting up the header and one question section where we ask for juliusbaer.com.
As we do know only the IP addresses of the
root servers
we ask l.root-servers.net: “Hey do you know something about juliusbaer.com?”. The server will
answer: “No I don’t, but I know who might know.” This basically results in the following chat

```
Us: "Hey l.root-servers.net do you know the IP of juliusbaer.com?"
l.root-servers.net: "No I don't but why don't you ask a.gtld-servers.net which you can find at 192.5.6.30"

Us: "Hey a.gtld-servers.net do you know the IP of juliusbaer.com?"
a.gtld-servers.net: "No I don't but why don't you ask a11-67.akam.net. I don't know his IP address though. But I know that a.gtld-servers.net might know. You can reach him at 192.5.630."

Us: "Damnit another name we need to resolve. Hey a.gtld-servers.net do you know the IP of a11-67.akam.net?"
a.gtld-servers.net: "Yes, it is 84.53.139.67."

Us: "Hey a11-67.akam.net do you know the IP of juliusbaer.com?"
a11-67.akam.net: "Yes, it is 188.92.48.169." 
```

And with that we have successfully resolved juliusbaer.com to 188.92.48.169. Simple no? If you want to double-check that
this is
correct run `nslookup juliusbaer.com` in your favorite terminal. For the full process of the resolution see also the
diagram below. Here you can see the full process in detail.

<figure>
  <img src="img/dns_process.svg" alt="DNS resolution process">
  <figcaption>Figure 4: The process of DNS resolution for juliusbaer.com</figcaption>
</figure>

# Bringing it to life

As we developers love to play around with languages and tools, why not transform the above chat into code that we can
reuse for all domain names? After all this is a great learning experience especially if we pair it with trying out a new
language.
For implementing protocols the only language of choice can of course only be... Erlang. Not only because it has its own
introduction movie on YouTube [Erlang: The Movie](https://www.youtube.com/watch?v=xrIjfIjssLE&t=6s)
but also because it supports binary pattern matching, more on this later. Greatly simplified our DNS resolver will have
the following
structure:

```
recursivelyResolve:
* build the DNS request
* send the data out over the wire
* consume the response
* Is it a response for our query? 
  * Yes -> return the IP address
  * No -> call recursivelyResolve again
```

Now let's translate this pseudocode into working Erlang code. First we have our entry resolve function that we call
recursively.
Here we build the DNS request, then send it out over the wire and then parse the incoming DNS response. Based on
the DNS response we continue. If the DNS response is an authoritative answer (header field AA=1), case 1, we can
immediately end
our search because we have found a result. If that is not the case and there are no additional records, we have run into
case 2. We just get back the name server who might know about juliusbaer.com
but not its IP address. So we need to do one more level of recursion. We now call the resolve method again to resolve
the name server. Once we have done this we can continue resolving juliusbaer.com with the obtained IP. In case 3 we get
additional
records for the name server that we can ask for juliusbaer.com in the additional resources section. We can immediately
then use that
IP to ask for juliusbaer.com.

```erlang
resolve(Domain, DnsServer) ->
  {ok, DnsRequest} = build_dns_request(Domain),
  {ok, DnsResponseUnparsed} = send_dns_request(DnsRequest, DnsServer, 53),
  {ok, DnsResponse} = parse_dns_response(DnsResponseUnparsed),

  %% Case 1: Authoritative answer
  if DnsResponse#dns_response.answer_type == 1 ->
    io:fwrite("~nAnswer found: ~p~n", [DnsResponse#dns_response.answer_records]),
    {ok, DnsResponse#dns_response.answer_records};

  %% Case 2: No authoritative answer and no additional resources
    length(DnsResponse#dns_response.additional_records) == 0 ->
      {ok, ParsedNameserver} = extract_name_server(DnsResponse),
      {ok, AnswerRecords} = resolve(ParsedNameserver, "199.7.83.42"),
      FirstAnswer = hd(lists:filter(fun(AnswerRecord) ->
        AnswerRecord#additional_record.type == 1 end, AnswerRecords)),
      NewDnsServerIp = FirstAnswer#additional_record.ip,
      resolve(Domain, NewDnsServerIp);

  %% Case 3: No authoritative answer with additional resources
    true ->
      {ok, OtherDnsServerIp} = extract_additional_resources(DnsResponse),
      io:fwrite("Making new call to: ~p~n", [OtherDnsServerIp]),
      resolve(Domain, OtherDnsServerIp)
  end.
```

With that we have built our own DNS resolver for resolving A records for domain names. Cool, isn't it? You can just take
an RfC and start coding as everything you need is so nicely described in the protocol definition. Of course, I left out
helper methods like `build_dns_request`, `send_dns_request` and others, but you can find them on
[GitHub](https://github.com/theotrama/dns_resolver). One more method I want to share here is the
`parse_header_section` method because it illustrates
how nice
Erlang is suited for implementing binary protocols. Remember the above definition of the DNS protocol
from [Figure 1](#figure-1)?
With
that definition in our hands we can write a one-liner to parse the entire header. We parse the first 16 bits into the ID
variable, the next 1
bit into the QR variable (is query/response 0/1),
the next 4 bits into the Opcode and then importantly the next bit into the AA variable (remember AA=1 -> we have our
authoritative answer and can stop) and so on. And with this one line of code we already have parsed the entire header.

```erlang
parse_header_section(Response) ->
  <<ID:16, QR:1, Opcode:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16, _/binary>> = Response,
  {ok, {ID, QR, Opcode, AA, TC, RD, RA, Z, RCODE, QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT}}.
```

## Checking it out

In order to play around with the DNS you can either use `nslookup` or `dig` or you can try out a small web app I built
that
documents the process of resolving a domain. You can go to [domainNameResolver](http://139.162.170.221/static) and paste
any domain
into the search field. From that you will get back the corresponding IPs and all the steps the DNS resolver took to
resolve
the domain you asked for.

## Additional resources

If you love to try out Erlang yourself I can only highly recommend the introduction book by the language's
founders https://erlang.org/download/erlang-book-part1.pdf. Other great articles on domain name resolution
are https://implement-dns.wizardzines.com/ and https://timothya.com/blog/dns/.