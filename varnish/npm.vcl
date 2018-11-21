#
# This is an example VCL file for Varnish.
#
# It does not do anything by default, delegating control to the
# builtin VCL. The builtin VCL is called when there is no explicit
# return statement.
#
# See the VCL chapters in the Users Guide at https://www.varnish-cache.org/docs/
# and https://www.varnish-cache.org/trac/wiki/VCLExamples for more examples.

# Marker to tell the VCL compiler that this VCL has been adapted to the
# new 4.0 format.
vcl 4.0;

import std;
backend default {
	.host = "127.0.0.1";
	.port = "4873";
}
sub vcl_recv {
    set req.http.X-Forwarded-For = client.ip;
}

sub vcl_backend_response {
  # Keep the response in cache for 4 hours if the response has
  # validating headers.
  if (beresp.http.ETag || beresp.http.Last-Modified) {
    set beresp.keep = 10m;
  }
}

sub vcl_deliver {
    set resp.http.X-Cache-Server = server.ip;
    unset resp.http.Via;
    unset resp.http.X-Varnish;
    # Add a header to indicate a cache HIT/MISS
    if (obj.hits > 0) {
       set resp.http.X-Cache = "HIT";
       set resp.http.X-Cache-Hits = obj.hits;
       set resp.http.X-Age = resp.http.Age;
       unset resp.http.Age;
    } else {
       set resp.http.X-Cache = "MISS";
    }
    return (deliver);
}