/*
 *  Adapted 2019-12-23 by Thomas Jakway from src/main/org/apache/tools/ant/types/Commandline.java
 *  Version 1.10.7. See https://ant.apache.org/ and included copy of required license.
 *  Ant license header follows.
 *
 *  ***********************************************************************
 *
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.apache.tools.ant.types;

import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * Commandline objects help handling command lines specifying processes to
 * execute.
 * <p>
 * The class can be used to define a command line as nested elements or as a
 * helper to define a command line by an application.
 * </p>
 * <pre>
 * &lt;someelement&gt;
 *   &lt;acommandline executable="/executable/to/run"&gt;
 *     &lt;argument value="argument 1"/&gt;
 *     &lt;argument line="argument_1 argument_2 argument_3"/&gt;
 *     &lt;argument value="argument 4"/&gt;
 *   &lt;/acommandline&gt;
 * &lt;/someelement&gt;
 * </pre>
 * The element <code>someelement</code> must provide a method
 * <code>createAcommandline</code> which returns an instance of this class.
 *
 */
public class Commandline {
    /**
     * Crack a command line.
     * @param toProcess the command line to process.
     * @return the command line broken into strings.
     * An empty or null toProcess parameter results in a zero sized array.
     */
    public static String[] translateCommandline(String toProcess) {
        if (toProcess == null || toProcess.isEmpty()) {
            //no command? no string
            return new String[0];
        }
        // parse with a simple finite state machine

        final int normal = 0;
        final int inQuote = 1;
        final int inDoubleQuote = 2;
        int state = normal;
        final StringTokenizer tok = new StringTokenizer(toProcess, "\"\' ", true);
        final ArrayList<String> result = new ArrayList<>();
        final StringBuilder current = new StringBuilder();
        boolean lastTokenHasBeenQuoted = false;

        while (tok.hasMoreTokens()) {
            String nextTok = tok.nextToken();
            switch (state) {
            case inQuote:
                if ("\'".equals(nextTok)) {
                    lastTokenHasBeenQuoted = true;
                    state = normal;
                } else {
                    current.append(nextTok);
                }
                break;
            case inDoubleQuote:
                if ("\"".equals(nextTok)) {
                    lastTokenHasBeenQuoted = true;
                    state = normal;
                } else {
                    current.append(nextTok);
                }
                break;
            default:
                if ("\'".equals(nextTok)) {
                    state = inQuote;
                } else if ("\"".equals(nextTok)) {
                    state = inDoubleQuote;
                } else if (" ".equals(nextTok)) {
                    if (lastTokenHasBeenQuoted || current.length() > 0) {
                        result.add(current.toString());
                        current.setLength(0);
                    }
                } else {
                    current.append(nextTok);
                }
                lastTokenHasBeenQuoted = false;
                break;
            }
        }
        if (lastTokenHasBeenQuoted || current.length() > 0) {
            result.add(current.toString());
        }
        if (state == inQuote || state == inDoubleQuote) {
            throw new TranslateCommandLineException("unbalanced quotes in " + toProcess);
        }
        return result.toArray(new String[result.size()]);
    }

    public static class TranslateCommandLineException
        extends RuntimeException {

        public TranslateCommandLineException(String msg) {
            super(msg);
        }
    }
}
