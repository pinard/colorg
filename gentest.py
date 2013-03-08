#!/usr/bin/env python

"""\
Handle testing and experiments for colorg.
"""

import json
import socket
import subprocess
import sys
import time


# How many seconds before trying again to contact a server.
retry_delta = .01
# How many seconds before giving up trying to contact a server.
retry_maximum = 1


class Main:
    verbose = False

    def main(self, *arguments):
        import getopt
        options, arguments = getopt.getopt(arguments, 'v')
        for option, value in options:
            if option == '-v':
                self.verbose = True

        Test()


class Server:
    server = None

    def __init__(self, host=None, port=None):
        self.host = host
        self.port = port

    def round_trip(self, structure):
        self.writer.write(json.dumps(structure) + '\n')
        return json.loads(self.reader.readline())

    def start(self):
        # Launch a server.
        arguments = ['colorg-server', '-q']
        if self.host is not None:
            arguments.append(self.host)
        if self.port is not None:
            arguments.append(str(self.port))
        self.server = subprocess.Popen(arguments)

        # The server needs time to start, kibitz for a connection.
        handle = socket.socket()
        retry = 0
        while retry < retry_maximum:
            try:
                handle.connect((self.host or 'localhost', self.port or 7997))
            except socket.error:
                time.sleep(retry_delta)
                retry += retry_delta
            else:
                if run.verbose:
                    sys.stderr.write("Server replied after %d ms.\n"
                                     % round(1000 * retry))
                break
        else:
            sys.exit("Server did not reply in %d ms.\n"
                     % round(1000 * retry))

        # Establish means to read and write to the server.
        self.reader = handle.makefile()
        self.writer = handle.makefile('w', 0)

        # Read server initial reply.
        greating = self.reader.readline()
        assert greating == '"colorg v1"\n'

    def stop(self):
        # Kill the server.
        self.server.terminate()
        del self.server

        # The server needs time to die, wait until this happens.
        handle = socket.socket()
        retry = 0
        while retry < retry_maximum:
            try:
                handle.connect((self.host or 'localhost', self.port or 7997))
            except socket.error:
                if run.verbose:
                    sys.stderr.write("Server died after %d ms.\n"
                                     % round(1000 * retry))
                break
            else:
                time.sleep(retry_delta)
                retry += retry_delta
        else:
            sys.exit("Server still alive after %d ms.\n"
                     % round(1000 * retry))


class Test:

    def __init__(self):
        server = Server()
        server.start()
        print '***', server.round_trip('poll')
        server.stop()


run = Main()
main = run.main

if __name__ == '__main__':
    main(*sys.argv[1:])
