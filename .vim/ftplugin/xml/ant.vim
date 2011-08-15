let g:xmldata_ant = {
\ 'vimxmlentities': [''],
\ 'vimxmlroot': ['project'],
\ 'ant': [
\ [],
\ { 'antfile': [], 'target': [], 'dir': []}
\ ],
\ 'available': [
\ [],
\ { 'resource': [], 'file': [], 'property': [], 'classname': []}
\ ],
\ 'chmod': [
\ [],
\ { 'src': [], 'perm': []}
\ ],
\ 'copydir': [
\ [],
\ { 'dest': [], 'src': [], 'filtering': [], 'excludes': [], 'defaultexcludes': [], 'ignore': [], 'includes': []}
\ ],
\ 'copyfile': [
\ [],
\ { 'dest': [], 'src': [], 'filtering': []}
\ ],
\ 'cvs': [
\ [],
\ { 'cvsroot': [], 'dest': [], 'tag': [], 'package': []}
\ ],
\ 'delete': [
\ [],
\ { 'file': []}
\ ],
\ 'deltree': [
\ [],
\ { 'dir': []}
\ ],
\ 'echo': [
\ [],
\ { 'message': []}
\ ],
\ 'exec': [
\ [],
\ { 'output': [], 'command': [], 'dir': [], 'os': []}
\ ],
\ 'expand': [
\ [],
\ { 'dest': [], 'src': []}
\ ],
\ 'filter': [
\ [],
\ { 'value': [], 'token': []}
\ ],
\ 'fixcrlf': [
\ [],
\ { 'cr': [], 'srcdir': [], 'destdir': [], 'tab': [], 'eof': [], 'excludes': [], 'includes': []}
\ ],
\ 'get': [
\ [],
\ { 'verbose': [], 'dest': [], 'src': []}
\ ],
\ 'gzip': [
\ [],
\ { 'src': [], 'zipfile': []}
\ ],
\ 'jar': [
\ [],
\ { 'jarfile': [], 'excludes': [], 'defaultexcludes': [], 'ignore': [], 'basedir': [], 'manifest': [], 'includes': [], 'items': []}
\ ],
\ 'java': [
\ [],
\ { 'args': [], 'classname': [], 'fork': [], 'jvmargs': []}
\ ],
\ 'javac': [
\ [],
\ { 'bootclasspath': [], 'excludes': [], 'defaultexcludes': [], 'classpath': [], 'extdirs': [], 'deprecation': [], 'destdir': [], 'srcdir': [], 'debug': [], 'filtering': [], 'optimize': [], 'includes': []}
\ ],
\ 'javadoc': [
\ [],
\ { 'verbose': [], 'docencoding': [], 'doctitle': [], 'locale': [], 'footer': [], 'encoding': [], 'stylesheetfile': [], 'nohelp': [], 'splitindex': [], 'nodedeprecatedlist': [], 'private': [], 'charset': [], 'classpath': [], 'old': [], 'overview': [], 'nonavbar': [], 'destdir': [], 'windowtitle': [], 'linkoffline': [], 'author': [], 'packagenames': [], 'group': [], 'extdirs': [], 'use': [], 'helpfile': [], 'public': [], 'nodedeprecated': [], 'link': [], 'serialwarn': [], 'bootclasspath': [], 'sourcepath': [], 'version': [], 'noindex': [], 'protected': [], 'bottom': [], 'package': [], 'notree': [], 'header': [], 'sourcefiles': []}
\ ],
\ 'keysubst': [
\ [],
\ { 'src': [], 'dest': [], 'keys': [], 'sep': []}
\ ],
\ 'mkdir': [
\ [],
\ { 'dir': []}
\ ],
\ 'project': [
\ ['target', 'property'],
\ { 'name': [], 'default': [], 'basedir': []}
\ ],
\ 'property': [
\ [],
\ { 'file': [], 'value': [], 'name': [], 'resource': []}
\ ],
\ 'rename': [
\ [],
\ { 'src': [], 'replace': [], 'dest': []}
\ ],
\ 'replace': [
\ [],
\ { 'file': [], 'value': [], 'token': []}
\ ],
\ 'rmic': [
\ [],
\ { 'filtering': [], 'base': [], 'classname': []}
\ ],
\ 'tar': [
\ [],
\ { 'defaultexcludes': [], 'includes': [], 'excludes': [], 'basedir': [], 'tarfile': []}
\ ],
\ 'target': [
\ ['ant', 'available', 'chmod', 'copydir', 'copyfile', 'cvs', 'delete', 'deltree', 'echo', 'exec', 'expand', 'filter', 'get', 'gzip', 'fixcrlf', 'jar', 'java', 'javac', 'javadoc', 'javadoc2', 'keysubst', 'mkdir', 'property', 'rename', 'replace', 'rmic', 'tar', 'taskdef', 'tstamp', 'zip', 'xt'],
\ { 'if': [], 'name': [], 'depends': []}
\ ],
\ 'taskdef': [
\ [],
\ { 'name': [], 'classname': []}
\ ],
\ 'tstamp': [
\ [],
\ { }
\ ],
\ 'xt': [
\ [],
\ { 'xml': [], 'out': [], 'xsl': []}
\ ],
\ 'zip': [
\ [],
\ { 'defaultexcludes': [], 'ignore': [], 'includes': [], 'zipfile': [], 'excludes': [], 'basedir': [], 'items': []}
\ ],
\ 'vimxmltaginfo': {
\ 'ant': ['/>', ''],
\ 'available': ['/>', ''],
\ 'chmod': ['/>', ''],
\ 'copydir': ['/>', ''],
\ 'copyfile': ['/>', ''],
\ 'cvs': ['/>', ''],
\ 'delete': ['/>', ''],
\ 'deltree': ['/>', ''],
\ 'echo': ['/>', ''],
\ 'exec': ['/>', ''],
\ 'expand': ['/>', ''],
\ 'filter': ['/>', ''],
\ 'fixcrlf': ['/>', ''],
\ 'get': ['/>', ''],
\ 'gzip': ['/>', ''],
\ 'jar': ['/>', ''],
\ 'java': ['/>', ''],
\ 'javac': ['/>', ''],
\ 'javadoc': ['/>', ''],
\ 'keysubst': ['/>', ''],
\ 'mkdir': ['/>', ''],
\ 'property': ['/>', ''],
\ 'rename': ['/>', ''],
\ 'replace': ['/>', ''],
\ 'rmic': ['/>', ''],
\ 'tar': ['/>', ''],
\ 'taskdef': ['/>', ''],
\ 'tstamp': ['/>', ''],
\ 'xt': ['/>', ''],
\ 'zip': ['/>', ''],
\ }
\ }
" vim:ft=vim:ff=unix