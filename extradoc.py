#!/usr/bin/env python

"""\
Extract documentation from one or more Python sources.
Documentation lies in all unprefixed, sextuple-quoted strings.

Usage: extradoc.py [OPTION]... [SOURCE]...

Options:
  -c PREFIX   Common prefix for all output files.
  -h          Produce an HTML file, name will be PREFIX.html.
  -o          Produce an Org file, name will be PREFIX.org.
  -p          Produce a PDF file, name will be PREFIX.pdf.
  -t          Produce a translation file, name will be PREFIX.pot.
  -v          Be verbose and repeat all of Emacs output.

If no SOURCE are given, the program reads and process standard input.
Option -c is mandatory.  If -h or -p are used and -o is not, file PREFIX.org
should not pre-exist, as the program internally writes it and then deletes it.
"""

import os
import polib
import re
import sys
import tokenize

encoding = 'UTF-8'


class Main:
    prefix = None
    html = False
    org = False
    pdf = False
    pot = False
    verbose = False

    def main(self, *arguments):

        # Decode options.
        import getopt
        options, arguments = getopt.getopt(arguments, 'c:hoptv')
        for option, value in options:
            if option == '-c':
                self.prefix = value
            if option == '-h':
                self.html = True
            elif option == '-o':
                self.org = True
            elif option == '-p':
                self.pdf = True
            elif option == '-t':
                self.pot = True
            elif option == '-v':
                self.verbose = True
        if not self.prefix:
            sys.exit("Option -c is mandatory.")
        if ((self.html or self.pdf) and not self.org
                and os.path.exists(self.prefix + '.org')):
            sys.exit("File %s.org exists and -o not specified." % self.prefix)

        # Prepare the work.
        if self.html or self.org or self.pdf:
            self.org_file = open(self.prefix + '.org', 'w')
        else:
            self.org_file = None
        if self.pot:
            self.po = polib.POFile()
            self.po.metadata = {
                'Project-Id-Version': '1.0',
                'Report-Msgid-Bugs-To': 'you@example.com',
                'POT-Creation-Date': '2007-10-18 14:00+0100',
                'PO-Revision-Date': '2007-10-18 14:00+0100',
                'Last-Translator': 'you <you@example.com>',
                'Language-Team': 'English <yourteam@example.com>',
                'MIME-Version': '1.0',
                'Content-Type': 'text/plain; charset=utf-8',
                'Content-Transfer-Encoding': '8bit',
                }

        # Process all source files.
        if arguments:
            for argument in arguments:
                self.extract_strings(file(argument), argument)
        else:
            self.extract_strings(sys.stdin, '<stdin>')

        # Complete the work.
        if self.pot:
            self.po.save(self.prefix + '.pot')
        if self.org_file is not None:
            self.org_file.close()
        if self.html:
            self.launch_emacs('html')
        if self.pdf:
            self.launch_emacs('pdf')
        if (self.html or self.pdf) and not self.org:
            os.remove(self.prefix + '.org')

    def extract_strings(self, file, name):
        for (code, text, (line, column), _, _
             ) in tokenize.generate_tokens(file.readline):
            if code == tokenize.STRING:
                if text.startswith('"""'):
                    text = text[3:-3].replace('\\\n', '')
                    if self.org_file is not None:
                        self.org_file.write(text)
                    if self.pot:
                        self.po.append(polib.POEntry(
                            msgid=text.decode(encoding),
                            occurrences=[(name, str(line))]))

    def launch_emacs(self, format):
        import subprocess
        for raw in subprocess.Popen(
                ['emacs', '--batch', self.prefix + '.org',
                 '--eval', '(setq org-export-allow-BIND t)',
                 '--eval', '(org-export-as-%s nil)' % format],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT).stdout:
            try:
                line = raw.decode(encoding)
            except UnicodeDecodeError:
                line = raw.decode('ISO-8859-1')
            if not self.verbose:
                match = re.match('^Loading .*/([^/]+)\\.cache\\.\\.\\.$',
                                 line)
                if match:
                    sys.stderr.write("%s\n" % match.group(1))
                    continue
                if line.startswith((
                        # Common
                        'Loading ',
                        'OVERVIEW',
                        'Saving file ',
                        'Wrote ',
                        # PDF
                        'Exporting to ',
                        'LaTeX export done, ',
                        'Processing LaTeX file ',
                        # Web
                        '(No changes need to be saved)',
                        '(Shell command succeeded ',
                        'Exporting...',
                        'HTML export done, ',
                        'Publishing file ',
                        'Resetting org-publish-cache',
                        'Skipping unmodified file ',
                        )):
                    continue
            sys.stderr.write(line)


run = Main()
main = run.main

if __name__ == '__main__':
    main(*sys.argv[1:])
