#!/usr/bin/python
import ldtp
import sys
import SimpleXMLRPCServer
import getopt
import logging
import re
import inspect
import wnck
import gobject
import gtk
import time
import fnmatch
import os
import subprocess
import pdb
import dogtail.tree
from fnmatch import translate

logger = logging.getLogger("xmlrpcserver.ldtp")
logger.setLevel(logging.INFO)

class LoggingSimpleXMLRPCRequestHandler(SimpleXMLRPCServer.SimpleXMLRPCRequestHandler):
  """Overides the default SimpleXMLRPCRequestHander to support logging.  Logs
  client IP and the XML request and response.
  """

  def do_POST(self):
    clientIP, port = self.client_address
    # Log client IP and Port
    logger.info('Client IP: %s - Port: %s' % (clientIP, port))
    try:
      # get arguments
      data = self.rfile.read(int(self.headers["content-length"]))
      # Log client request
      logger.info('Client request: \n%s\n' % data)

      response = self.server._marshaled_dispatch(data, getattr(self, '_dispatch', None))
      # Log server response
      logger.info('Server response: \n%s\n' % response)
    except:
      # This should only happen if the module is buggy
      # internal error, report as HTTP server error
      self.send_response(500)
      self.end_headers()
    else:
      # got a valid XML RPC response
      self.send_response(200)
      self.send_header("Content-type", "text/xml")
      self.send_header("Content-length", str(len(response)))
      self.end_headers()
      self.wfile.write(response)

      # shut down the connection
      self.wfile.flush()
      self.connection.shutdown(1)

#figure out which methods are in LDTPv2 and only use those
#f = open("/root/bin/ldtp_api2.clj", "r")
#ldtp2commands = []
#line = f.readline().strip()
#while line:
#  command = line.split("\"")[1]
#  ldtp2commands.append(command)
#  line = f.readline()
#ldtp2commands.sort()
#f.close

ldtp3commands = ['activatetext',
                 'activatewindow',
                 'appendtext',
                 'appundertest',
                 'check',
                 'checkrow',
                 'click',
                 'closewindow',
                 'comboselect',
                 'comboselectindex',
                 'copytext',
                 'cuttext',
                 'decrease',
                 'delaycmdexec',
                 'deletetext',
                 'deregisterevent',
                 'deregisterkbevent',
                 'doesmenuitemexist',
                 'doesrowexist',
                 'doubleclick',
                 'doubleclickrow',
                 'enterstring',
                 'expandtablecell',
                 'generatekeyevent',
                 'generatemouseevent',
                 'getaccesskey',
                 'getallitem',
                 'getallstates',
                 'getapplist',
                 'getcellsize',
                 'getcellvalue',
                 'getcharcount',
                 'getchild',
                 'getcombovalue',
                 'getcpustat',
                 'getcursorposition',
                 'getlastlog',
                 'getmax',
                 'getmaxvalue',
                 'getmemorystat',
                 'getmin',
                 'getminincrement',
                 'getminvalue',
                 'getobjectinfo',
                 'getobjectlist',
                 'getobjectnameatcoords',
                 'getobjectproperty',
                 'getobjectsize',
                 'getpanelchildcount',
                 'getrowcount',
                 'getslidervalue',
                 'getstatusbartext',
                 'gettabcount',
                 'gettablerowindex',
                 'gettabname',
                 'gettextvalue',
                 'getvalue',
                 'getwindowlist',
                 'getwindowsize',
                 'grabfocus',
                 'guiexist',
                 'guitimeout',
                 'handletablecell',
                 'hasstate',
                 'hidelist',
                 'imagecapture',
                 'increase',
                 'inserttext',
                 'invokemenu',
                 'isalive',
                 'ischildindexselected',
                 'ischildselected',
                 'istextstateenabled',
                 'keypress',
                 'keyrelease',
                 'launchapp',
                 'listsubmenus',
                 'maximizewindow',
                 'menucheck',
                 'menuitemenabled',
                 'menuuncheck',
                 'minimizewindow',
                 'mouseleftclick',
                 'mousemove',
                 'mouserightclick',
                 'objectexist',
                 'objtimeout',
                 'onedown',
                 'oneleft',
                 'oneright',
                 'oneup',
                 'onwindowcreate',
                 'pastetext',
                 'poll_events',
                 'press',
                 'registerevent',
                 'registerkbevent',
                 'remap',
                 'removecallback',
                 'rightclick',
                 'scrolldown',
                 'scrollleft',
                 'scrollright',
                 'scrollup',
                 'selectall',
                 'selecteditemcount',
                 'selectindex',
                 'selectitem',
                 'selectlastrow',
                 'selectmenuitem',
                 'selectpanel',
                 'selectpanelindex',
                 'selectpanelname',
                 'selectrow',
                 'selectrowindex',
                 'selectrowpartialmatch',
                 'selecttab',
                 'selecttabindex',
                 'setcellvalue',
                 'setcursorposition',
                 'setlocale',
                 'setmax',
                 'setmin',
                 'settextvalue',
                 'setvalue',
                 'showlist',
                 'simulatemousemove',
                 'singleclickrow',
                 'startprocessmonitor',
                 'stateenabled',
                 'stopprocessmonitor',
                 'uncheck',
                 'uncheckrow',
                 'unhandletablecell',
                 'unmaximizewindow',
                 'unminimizewindow',
                 'unselectall',
                 'unselectindex',
                 'unselectitem',
                 'verifycheck',
                 'verifydropdown',
                 'verifyhidelist',
                 'verifymenucheck',
                 'verifymenuuncheck',
                 'verifypartialmatch',
                 'verifypartialtablecell',
                 'verifypushbutton',
                 'verifyscrollbarhorizontal',
                 'verifyscrollbarvertical',
                 'verifyselect',
                 'verifysettext',
                 'verifysetvalue',
                 'verifyshowlist',
                 'verifysliderhorizontal',
                 'verifyslidervertical',
                 'verifytablecell',
                 'verifytabname',
                 'verifytoggled',
                 'verifyuncheck',
                 'wait',
                 'waittillguiexist',
                 'waittillguinotexist',
                 'windowuptime']

_ldtp_methods = filter(lambda fn: inspect.isfunction(getattr(ldtp,fn)),  dir(ldtp))
_supported_methods = filter(lambda x: x in ldtp3commands, _ldtp_methods)
#_unsupported_methods = filter(lambda x: x not in ldtp3commands, _ldtp_methods)
_additional_methods = ['closewindow', 'maximizewindow']
for item in _additional_methods: _supported_methods.append(item)
_supported_methods.sort()

#create a class with all ldtp methods as attributes
class AllMethods:
  #states enum from /usr/include/at-spi-1.0/cspi/spi-statetypes.h as part of at-spi-devel
  states = ['INVALID',
            'ACTIVE',
            'ARMED',
            'BUSY',
            'CHECKED',
            'COLLAPSED',
            'DEFUNCT',
            'EDITABLE',
            'ENABLED',
            'EXPANDABLE',
            'EXPANDED',
            'FOCUSABLE',
            'FOCUSED',
            'HORIZONTAL',
            'ICONIFIED',
            'MODAL',
            'MULTI_LINE',
            'MULTISELECTABLE',
            'OPAQUE',
            'PRESSED',
            'RESIZABLE',
            'SELECTABLE',
            'SELECTED',
            'SENSITIVE',
            'SHOWING',
            'SINGLE_LINE',
            'STALE',
            'TRANSIENT',
            'VERTICAL',
            'VISIBLE',
            'MANAGES_DESCENDANTS',
            'INDETERMINATE',
            'TRUNCATED',
            'REQUIRED',
            'INVALID_ENTRY',
            'SUPPORTS_AUTOCOMPLETION',
            'SELECTABLE_TEXT',
            'IS_DEFAULT',
            'VISITED',
            'LAST_DEFINED']

  def _translate_state(self, value):
    if value in self.states:
      return self.states.index(value)
    else:
      return value

  def _translate_number(self, num):
    if num in xrange(len(self.states)):
      return self.states[num]
    else:
      return num

  def _getobjectproperty(self, window, object):
    getobjectlist = getattr(ldtp,"getobjectlist")
    objects = getobjectlist(window)
    for item in objects:
      if re.search(object,str(item)):
        return str(item)
    return object

  def _matches(self, pattern, item):
    return bool(re.match(fnmatch.translate(pattern), item, re.M | re.U | re.L))

  #this replicates the origional algorithm
  def _gettablerowindex(self, window, table, target):
    numrows = ldtp.getrowcount(window, table)
    numcols = len(ldtp.getobjectproperty(window, table, 'children').split())
    for i in range(0,numrows):
      for j in range(0,numcols):
        try:
          value = ldtp.getcellvalue(window, table, i, j)
          if self._matches(target,value):
            ldtp.selectrowindex(window, table, i)
            return i
        except:
          continue
    raise Exception("Item not found in table!")

  #this only searches the first column and is much quicker.
  def _quickgettablerowindex(self, window, table, target):
    numrows = ldtp.getrowcount(window, table)
    for i in range(0,numrows):
      try:
        value = ldtp.getcellvalue(window, table, i, 0)
        if self._matches(target,value):
          ldtp.selectrowindex(window, table, i)
          return i
      except:
        continue
    raise Exception("Item not found in table!")

  def _window_search(self, match, term):
    if re.search(fnmatch.translate(term),
                   match,
                   re.U | re.M | re.L) \
          or re.search(fnmatch.translate(re.sub("(^frm|^dlg)", "", term)),
                       re.sub(" *(\t*)|(\n*)", "", match),
                       re.U | re.M | re.L):
      return True
    else:
      return False

  def _closewindow(self, window_name):
    screen = wnck.screen_get_default()
    while gtk.events_pending():
      gtk.main_iteration()

    windows = screen.get_windows()
    success = 0
    for w in windows:
      current_window = w.get_name()
      if self._window_search(current_window,window_name):
        w.close(int(time.time()))
        success = 1
        break

    gobject.idle_add(gtk.main_quit)
    gtk.main()
    return success

  def _maximizewindow(self, window_name):
    screen = wnck.screen_get_default()
    while gtk.events_pending():
      gtk.main_iteration()

    windows = screen.get_windows()
    success = 0
    for w in windows:
      current_window = w.get_name()
      if self._window_search(current_window,window_name):
        w.maximize()
        success = 1
        break

    gobject.idle_add(gtk.main_quit)
    gtk.main()
    return success

  def _launchapp(self, cmd, args=[], delay=0, env=1, lang="C"):
    os.environ['NO_GAIL']='0'
    os.environ['NO_AT_BRIDGE']='0'
    if env:
      os.environ['GTK_MODULES']='gail:atk-bridge'
      os.environ['GNOME_ACCESSIBILITY']='1'
    if lang:
      os.environ['LANG']=lang
    try:
      process=subprocess.Popen([cmd]+args, close_fds=True)
      # Let us wait so that the application launches
      try:
        time.sleep(int(delay))
      except ValueError:
        time.sleep(5)
    except Exception, e:
      raise Exception(str(e))
    os.environ['NO_GAIL']='1'
    os.environ['NO_AT_BRIDGE']='1'
    return process.pid

  def _gettextvalue(self, window_name, object_name, startPosition=None,
                    endPosition=None):
    def findFirst(node, search_string):
      try: children = node.children
      except: return None
      for child in children:
        if self._matches(search_string, child.name):
          return child
        else:
          child = findFirst(child,search_string)
          if child:
            return child
    retval = ""
    if ldtp.getobjectproperty(window_name, object_name, "class") == "label":
      f = dogtail.tree.root.application('subscription-manager-gui')
      w = f.childNamed(window_name)
      o = findFirst(w, object_name)
      if o:
       retval = o.text
      else:
       raise Exception("Cannot find object: %s in tree."%(object_name))
    else:
      retval = ldtp.gettextvalue(window_name, object_name, startPosition, endPosition)
    if not ((isinstance(retval, str) or isinstance(retval, unicode))):
      retval = ""
    return retval

  def _dispatch(self, method, params):
    if method in _supported_methods:
      paramslist = list(params)
      if method == "closewindow":
        return self._closewindow(paramslist[0])
      elif method == "getobjectproperty":
        paramslist[1] = self._getobjectproperty(paramslist[0],paramslist[1])
        params = tuple(paramslist)
      elif method == "gettextvalue":
        return self._gettextvalue(*paramslist)
      elif method == "hasstate":
        paramslist[2]=self._translate_state(paramslist[2])
        params = tuple(paramslist)
      elif method == "launchapp":
        return self._launchapp(*paramslist)
      elif method == "maximizewindow":
        return self._maximizewindow(paramslist[0])

      function = getattr(ldtp,method)
      retval = function(*params)

      if (retval == -1) and (method == "gettablerowindex"):
        paramslist = list(params)
        #use quick method for now
        retval = self._quickgettablerowindex(paramslist[0],
                                             paramslist[1],
                                             paramslist[2])
      elif method == "getallstates":
        retval = [self._translate_number(state) for state in retval]

      if retval == None:
        retval = 0

      return retval
  pass

for name in _supported_methods:
  if not item in _additional_methods:
    setattr(AllMethods, name, getattr(ldtp, name))

def usage():
  print "Usage:"
  print "[-p, --port=] Port to listen on"
  print "[-l --logfile=] file to write logging to"
  print "[-h] This help message"

def start_server(port,logfile):
  if logfile:
    hdlr = logging.FileHandler(logfile)
    formatter = logging.Formatter("%(asctime)s  %(levelname)s  %(message)s")
    hdlr.setFormatter(formatter)
    logger.addHandler(hdlr)
    server = SimpleXMLRPCServer.SimpleXMLRPCServer(("",int(port)),
                                                    LoggingSimpleXMLRPCRequestHandler)
  else:
    server = SimpleXMLRPCServer.SimpleXMLRPCServer(('',int(port)),
                                                    logRequests=True)

  server.register_introspection_functions()
  server.register_instance(AllMethods())

  try:
    print("Listening on port %s" % port)
    server.serve_forever()
  except KeyboardInterrupt:
    print 'Exiting'

def main():
  try:
    opts, args = getopt.getopt(sys.argv[1:], "hpl:v", ["help", "port=", "logfile="])
    print(opts)
  except getopt.GetoptError, err:
    # print help information and exit:
    print str(err) # will print something like "option -a not recognized"
    usage()
    sys.exit(2)

  port = 4118 #default port
  logfile = None

  for o, a in opts:
    if o in ("-p", "--port"):
      port = a
    elif o in ("-l", "--logfile"):
      logfile = a
    elif o in ("-h", "--help"):
        usage()
        sys.exit()
    else:
        assert False, "unhandled option"

  start_server(port,logfile)

if __name__ == "__main__":
    main()
