#! /usr/bin/env python
##
## Routine to make it easy to read command line arguments in a keyword=value
## style format.  Also has support for --help, help=, -h, etc, and setting
## a debug level.  Finally, includes the warning, error, and dprintf functions
## which may also be useful in scripting.
##
##   15-mar-2003   Created                                   PJT
##   16-apr-2003   added run,keyr,keyi,keya
##   05-mar-2004   Added help comments nemo style            NLC
##   16-may-2004   Deleted all the code we don't use for map2  NLC
##   19-feb-2008   Changed keya, keyi, and keyr to not use the 
##                 keypresent function.  This allows the programmer to
##                 specify a default keyword value of nothing.  However,
##                 error checking was added to keyini so that if the user
##                 tries to input an empty keyword on the command line,
##                 than an error message will still occur.  Also, some cleanups
##                 to remove extraneous comments, combined the badkeyword
##                 function with keya, and added the keyf function which works
##                 the same as keyr.
##   28-jul-2008   merged map2's miriad.py in miriad'd Miriad.py      PJT
##   30-jul-2008   moved back old useful functions for Miriad         PJT
##
##  Example usage:
##    help = """
##           bla bla
##           """
##    keyval = {
##       "key1" :  "val1\n  help1",
##       "key2" :  "val2\n  help2"
##       }
##
##    import Miriad
##    Miriad.keyini(keyval,usage=help)
##    a = Miriad.keya('key1')

##
import sys, os, string

#   some global variables (private to Miriad.py)
_version    = "2.0 (28-jul-2008)"
_quit       = 0
_mkeyval    = {}
_help       = {}
_debuglevel = 0
_logger     = ""

def run(command,log,fatal):
    """an alias for miriad()"""
    return miriad(command,log,fatal)

def miriad(command,log=0,fatal=1):
    """
    miriad        execute a miriad 'command' (as a list of strings) and accumulate a log
                  either from the default logfile (see setlogger) or by overriding using
                  log=. By default an error results into an exit, though this can be
                  overriden by using fatal=0
                  Example:    miriad(['itemize','in=ngc1365.cm'],log='ngc1365.log')

    Other commands available in PYRAMID are:
    
    doc           show specific help for a MIRIAD task , e.g. doc('invert')
    keys          show all keywords for a MIRIAD task , e.g. keys('invert')
    setlogger     change the default logger for subsequent miriad() commands,
                  e.g. setlogger('rubbish.log')

    zap           delete a miriad dataset (with existence check), e.g. zap('ngc1365.cm')
    zap_all       delate a set of data (unchecked), e.g. zap_all('ncg1365*')
    grepcmd       Return the Nth word on the first occurence of a string match from a command
                  e.g.:    grepcmd('histo in=ngc1365.cm','Maximum',2)
    greplog       Return the Nth word on the first occurence of a string match from a logfile
                  e.g.:    grepcmd('ngc1365.log','naxis1',2)
                  Notice that the index N starts at 0!!

    Some useful classes defined via PYRAMID:

    Timer         mark and compute CPU times between sections of the code

    A number of useful and less useful user contribued PYRAMID scripts
    can be found in and below $MIR/examples/
    
    """
    global _logger
    if (log != 0):
        mycmd = cmd(command) + '> %s 2>&1' % log
    else:
        mycmd = cmd(command) + _logger;
    # debug
    print "MIRIAD% ",mycmd
    retval = os.system(mycmd)
    if retval:
        print "###: Error %d from %s" % (retval,command[0])
        if fatal:
            os._exit(retval)

def setlogger(log,append=0):
    """ set the logfile for the miriad() function. By default any old logfile is removed"""
    global _logger
    print "setlogger: default logfile now " + log
    _logger = '>> %s 2>&1' % log
    if append==0:
        zap(log)



def keyini(keyval,show=0,gui=0,usage=None):
    """Initialize the values of all keywords, and check to see that
       all required keywords are present.  Also, it user specifies
       help, it calls the show_keyval function

       @param keyval A dictionary of keywords and values (special: _msg_)
       @param help   A helpful usage reminder what the program does
       @param show   If 1, calls show_keyval
       @param gui    If 1, ignores required keywords (???)

       @return None"""
    global _quit, _mkeyval, _help, _debuglevel

    _help = keyval.copy()
    for key in _help.keys():  # Parse for help comments if any
       idx = _help[key].find('\n')
       if idx >= 0: # Found help comment
          _help[key] = _help[key][idx+1:].strip()
          keyval[key] = keyval[key][:idx].strip()
       elif key == '_msg_':
          # deprecated, should use the originally intended help
          pass # do nothing since this is the usage statement
       else: # No help comment found
          _help[key] = ""
    # old way of passing usage statement as a separate string.
    if usage:
       _help['_msg_']= usage
    
    # Set default debug level to 0
    _debuglevel = 0
    for arg in sys.argv[1:]:
        i=arg.find("=")
        if arg in ["--help","-h","help"]:
            _quit=1
        elif i > 0:
            key = arg[0:i]
            val = arg[i+1:]
            if len(val) == 0:
               error('keyword %s cannot have a blank value!' %key)
            elif keyval.has_key(key):
                keyval[key] = val
            elif key == "help":
                _quit=1
            elif key == 'debug':
               try:
                  _debuglevel = int(val)
               except ValueError:
                  error('debug=%s must be an integer' %val)
            else:
                error("keyword %s does not exist, try --help or -h" % arg)
        else:
            warning("argument %s not understood, try --help or -h" % arg)
            warning("Skipping and continuing.")
    _mkeyval = keyval.copy() # keep a reference
    if _quit:
        show_keyval(_mkeyval,_help,_quit)
    elif show:
        show_keyval(_mkeyval,_help,_quit)
    if not gui:
       check_required()

def check_required():
   """Checks to see that required arguments (default values of ???) have
   been given new values"""
   global _mkeyval
   usage="Usage: %s " %sys.argv[0]
   _missing=0

   if '???' in _mkeyval.values():
      print "### Fatal Error! Insufficient parameters.  Try --help or -h."
      for i in _mkeyval.keys():
         if _mkeyval[i]=='???':
            usage=usage+"%s=??? " %i
            _missing=_missing+1
      if len(_mkeyval.keys())>_missing:
         usage=usage+"..."
      print usage
      os._exit(0)

def at_file(filename):
   """Tries to read an at-file, a file that contains keyword values.
      Specified by key=@filename.  It converts the file to a
      string of comma separated values.

      @param filename - string name of file.  Assumed to still
      	 contain the leading @ symbol"""

   if os.path.exists(filename[1:]):
      fp = open(filename[1:],'r')
      data=fp.readlines()
      fp.close()
      val=''
      for x in data:
      	 val=val+x[0:-1]+','
      val = val[0:-1] # delete trailing comma
   else:
      error("Keyword value file %s is missing!" %filename[1:])
   return val

def debug():
   '''Gets the current debug level and returns it'''
   return _debuglevel

def keya(key):
   """return keyword value as a string and will give an error message if they
      keyword does not exist."""
   
   if _mkeyval.has_key(key):
      return _mkeyval[key]
   else:
      error('Invalid keyword "%s"!' %key)

def keyi(key):
   """return keyword value as integer"""

   try:
      return int(keya(key))
   except ValueError:
      error("%s is not a valid integer for keyword %s!" %(keya(key),key))

def keyr(key):
   """return keyword value as a float"""   
   try:
      return float(keya(key))
   except ValueError:
      error("%s is not a valid float for keyword %s!" %(keya(key),key))

def keyf(key):
   """A duplicate for the keyr function since this mnemonic may make sense
      to the user."""
   keyr(key)

def keyl(key,val='a'):
   """return keyword value as a list.
   
      The val keyword can be either a (ascii), i (integer), r, or f (float)"""

   temp=keya(key)
   if temp!='':
      if temp[0]=='@':
         temp=at_file(temp).split(',')
      else:
         temp=temp.split(',')
      if val=='i':
         for i in range(0,len(temp)):
            try:
               temp[i]=int(temp[i])
            except:
               error("%s is not a valid integer!" %temp[i])
      elif val=='r' or val == 'f':
         for i in range(0,len(temp)):
            try:
               temp[i]=float(temp[i])
            except:
               error("%s is not a valid float!" %temp[i])
   return temp

def keyb(key):
   """Return keyword value as a boolean True/False.
   
      Can understand True,False,1,0,yes, and no as all valid."""
   temp = keya(key)
   if temp.lower() in ('1','yes','true','y', 't'):
      return True
   elif temp.lower() in ('0','no','false','n', 'f'):
      return False
   else:
      error("%s is not a boolean value!" %temp)
   
def show_keyval(keyval,help=0,quit=0):
    """Print out the current keywords, their values and a help message, if
       one is present."""

    if help!=0:
      if help.has_key('_msg_'):
      	 print help['_msg_']
    print "------------------------------------------------------------"
    for k in keyval.keys():
       if k != '_msg_':   
          if help != 0:
             print k + '=' + keyval[k] + '  '+help[k]
          else:
             print k + '=' + keyval[k]
    print "------------------------------------------------------------"
    if quit:
        os._exit(0)

def version():
    # print "Miriad.py: version %s" % _version
    return _version;


def warning(text):
   """Print a string of text as a warning"""
   sys.stderr.write("### Warning! %s\n" %text)

def error(text):
   """Print a string of text as a fatal error and quit"""
   sys.stderr.write("### Fatal Error! %s\n" %text)
   sys.exit()

def dprint(level,text):
   """Print the debug message text if level is <= _debuglevel"""
   
   try:
      x = int(level)
   except ValueError:
      error('%s is not a valid integer' %str(level))
   if x <= _debuglevel:
      print text

def zap(file):
    """ remove a possibly existing miriad dataset (no wildcards)"""
    if os.path.isdir(file):
        os.system('rm -rf ' + file)
    elif os.path.isfile(file):
        os.system('rm -f ' + file)

def zap_all(files):
    """ remove all datasets (wildcards now allowed via the shell)"""
    os.system('rm -rf ' + files)

def cmd(cmdlist,debug=0):
    """convert a list of strings to a command string"""
    str = cmdlist[0] 
    for arg in cmdlist[1:]:
        str = str + ' ' +  arg
    if debug: print str
    return str

def doc(task):
    """show a little help on a miriad task...."""
    os.system('doc %s | less' % task)

def keys(task):
    """show just the keywords for a miriad task"""
    os.system('doc %s | grep ^Keyword' % task)

def timer(slot):
    """ save CPU info in predefined global slots"""
    global timet, timec
    timec[slot] = time.clock()
    timet[slot] = time.time()

def cpulen(a,b):
    """create a string to print human readable CPU info between slots """
    st = '%.3f ' % (timet[b]-timet[a])
    sc = '%.3f ' % (timec[b]-timec[a])
    return sc + st

#   should use popen()
def grepcmd(cmd,word,index=0):
    """run a command, grep through the output for a word and return a 0-indexed word"""
    log = 'tmp.log'
    os.system(cmd + ' > %s' % log)
    f = open(log,"r")
    v = f.read()
    va=string.split(v,"\n")
    f.close()
    for s in va:
        if string.find(s,word)>=0:
            sa=string.split(s)
            # print 'Match ' + word + '=>' + sa[index]
            return sa[index]
    print 'No match on' + word
    return "no-match"

def greplog(log,word,index=0):
    """ grep through a logfile for a word and return a 0-indexed word"""
    f = open(log,"r")
    v = f.read()
    va=string.split(v,"\n")
    f.close()
    for s in va:
        if string.find(s,word)>=0:
            sa=string.split(s)
            # print 'Match ' + word + '=>' + sa[index]
            return sa[index]
    print 'No match on' + word
    return "no-match"

class Timer:
    """a class to help you compute CPU times the script takes
    between certain tagged locations, e.g.
              t=Timer()
              t.tag()
              .....
              t.tag()
              print "Time passed %g seconds" % t.dt(0,1)
    """
    
    def __init__(self):
        self.n = 0
        self.timec = []
        self.timet = []
    def count(self):
        return self.n
    def tag(self):
        self.n = self.n + 1
        self.timec.append(time.clock())
        self.timet.append(time.time())
    def show(self):
        print "Currently have %d entries" % self.n
        print time.clock()
        print time.time()
    def showall(self):
        print self.timec
        print self.timet
    def clock(self,i):
        return self.timec[i]
    def time(self,i):
        return self.timet[i]
    def dt(self,i,j):
        return self.timet[j]-self.timet[i]
    def dc(self,i,j):
        return self.timec[j]-self.timec[i]
    

#   If executed... probably they are in an interactive python shell
if __name__ == '__main__':
   print """This program provides commandline argument reading
   for the various python programs.  It is not intended to be
   used as a standalone program."""
