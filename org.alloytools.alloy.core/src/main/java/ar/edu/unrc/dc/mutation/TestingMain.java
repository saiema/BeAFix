package ar.edu.unrc.dc.mutation;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;

import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import edu.mit.csail.sdg.ast.Browsable;
import edu.mit.csail.sdg.ast.Expr;
import edu.mit.csail.sdg.ast.Sig;
import edu.mit.csail.sdg.ast.Sig.Field;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.parser.CompUtil;

public class TestingMain {


    public static void main(String[] args) {
        MutationConfiguration mconfig = MutationConfiguration.getInstance();
        mconfig.setConfig(ConfigKey.OPERATOR_BES_TYPE_CHECK, Boolean.TRUE);
        System.out.println(mconfig.getConfigValue(ConfigKey.OPERATOR_BES_TYPE_CHECK));

        if (args.length != 1)
            throw new IllegalArgumentException("Only one argument expected");
        if (args[0].isEmpty())
            throw new IllegalArgumentException("File name is empty");
        if (!Files.isRegularFile(Paths.get(args[0])))
            throw new IllegalArgumentException("File doesn't exist");

        String pathToFile = args[0];

        CompModule mainModule = CompUtil.parseEverything_fromFile(null, null, pathToFile);

        List<Expr> expressions = findExpressions(mainModule);

        List<Mutation> mutations = new LinkedList<>();

        for (Expr e : expressions) {
            for (Ops o : Ops.values()) {
                if (o.isImplemented()) {
                    Optional<List<Mutation>> opMutations = o.getOperator().getMutations(e);
                    if (opMutations.isPresent())
                        mutations.addAll(opMutations.get());
                }
            }
        }

        System.out.println(mutations);


    }

    private static List<Expr> findExpressions(CompModule m) {
        List<Expr> expressions = new LinkedList<>();
        Queue<Browsable> nodesToVisit = new LinkedList<>();
        nodesToVisit.add(m);
        while (!nodesToVisit.isEmpty()) {
            Browsable n = nodesToVisit.poll();
            if (n instanceof Expr && !filter((Expr) n))
                expressions.add((Expr) n);
            nodesToVisit.addAll(n.getSubnodes());
        }
        return expressions;
    }

    private static boolean filter(Expr e) {
        if (e instanceof Sig)
            return true;
        if (e instanceof Field)
            return true;
        return false;
    }

}
