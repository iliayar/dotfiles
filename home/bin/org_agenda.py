#!/usr/bin/env python

import subprocess

class AgendaParser:
    
    def __init__(self):

        with open('/home/iliayar/.emacs.d/print-agenda.el', 'r') as f:
            print_agenda_script = f.read()
        
        
        subprocess.run(['emacsclient', '-e', print_agenda_script])
        
        with open('/tmp/agenda.txt', 'r') as f:
            def parse_line(line):
                line = line.split()
                i = 0
                file = line[i][:-1]
                i += 1
        
                time = line[i].strip('.')
                i += 1
                if time.endswith('-'):
                    time += line[i]
                    i += 1
            
                category = line[i][:-1]
                i += 1
        
                name = ' '.join(line[i:])
                return (file, time, category, name)
            predicate = lambda line: ('Scheduled' in line or 'Deadline' in line) and 'DONE' not in line
            self.agenda = map(parse_line, filter(predicate, f.read().split('\n')))
    def list(self):
        return list(self.agenda)

AgendaParser.FILE     = 0
AgendaParser.TIME     = 1
AgendaParser.CATEGORY = 2
AgendaParser.TEXT     = 3
