package clojai;

import com.springrts.ai.oo.*;
import java.lang.reflect.*;

class Loader extends OOAIFactory
{
    public OOAI createAI(int teamId, OOAICallback callback) 
    {
        try {
            System.out.println("Initialising Clojure runtime...");            

            //
            // Clojure uses the context classloader, but Spring wants us to
            // use a different one, so lets set it up here.
            //
            ClassLoader cl = this.getClass().getClassLoader();        
            Thread.currentThread().setContextClassLoader(cl);
            
            //
            // Java's reflection syntax is soooo ugly.
            //
            Class rt = cl.loadClass("clojure.lang.RT");            
            
            //
            // just does: (load 'clojai.main)
            //
            Method load = rt.getMethod("load", new Class[] {String.class, Boolean.TYPE});            
            load.invoke(null, new Object[] {"clojai/main", true});

            //
            // just does: (clojai.main/create-ai-proxy teamId callback)
            //
            Method var = rt.getMethod("var", new Class[] {String.class, String.class});
            Object fn = var.invoke(null, new Object[] {"clojai.main", "create-ai-proxy"});            
            Method createAiProxy = fn.getClass().getMethod("invoke", new Class[] {Object.class, Object.class});
            return (OOAI) createAiProxy.invoke(fn, new Object[] {teamId, callback});

        } catch (Exception e) {
            throw new RuntimeException("unable to initialise Clojure or clojai/core.", e);            
        }                
    }    
}
