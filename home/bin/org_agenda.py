#!/usr/bin/env python

import subprocess
import io
import csv

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

        script = '(progn (require (quote org)) (setq org-agenda-files (quote ("/home/iliayar/Dropbox/org"))) (org-batch-agenda-csv "a"))'
        proc = subprocess.Popen(['/usr/bin/emacs', '-batch', '--eval', script], stdout=subprocess.PIPE)

        reader = csv.reader(io.TextIOWrapper(proc.stdout, 'utf-8'))
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
