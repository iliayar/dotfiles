#!/usr/bin/env python

import subprocess
import io
import csv
import re


def call_lisp(cmd):
    proc = subprocess.Popen(['/usr/bin/emacs', '-batch', '-l', '~/bin/lisp/load-org', '-Q', '--eval', cmd], stdout=subprocess.PIPE)
    return io.TextIOWrapper(proc.stdout, 'utf-8')

def mark_done(file, title):
    call_lisp('(mark-done "{}" "{}")'.format(title, file)).close()

class HeadlineParser:

    def __init__(self, title):
        reader = call_lisp('(get-study-headline "{}")'.format(title))

        content = ''
    
        line = reader.readline()
        while not line.startswith('**'):
            line = reader.readline()
            if line == '':
                break
        line = reader.readline()
        while not line.startswith('***'):
            content += re.sub(r'\[\[(.*)\]\[(.*)\]\]', r'<a href="\1">\2</a>', line)
            line = reader.readline()
            if line == '':
                break

        self.content = content
    def get_content(self):
        return self.content

class AgendaEntry:

    def __init__(self, row):
        self.file = row[0]
        self.text = row[1]
        self.type = row[2]
        self.status = row[3]
        self.date = row[5]
        self.time = row[6].strip('.')
        self.priority = row[9]


class AgendaParser:
    
    def __init__(self):
    
        if AgendaParser.data != None:
            return

        reader = csv.reader(call_lisp('(batch-all-agenda)'))
        data = []
        for row in reader:
            entry = AgendaEntry(row)
            if entry.status == 'DONE' or not (entry.type in ['deadline', 'scheduled']):
                continue
            data += [entry]
        AgendaParser.data = data
        
    def get_iter(self):
        return iter(AgendaParser.data)
    def list(self):
        return AgendaParser.data
        

AgendaParser.data = None
AgendaParser.FILE     = 0
AgendaParser.TIME     = 1
AgendaParser.CATEGORY = 2
AgendaParser.TEXT     = 3
