package ar.edu.unrc.dc.mutation;

import ar.edu.unrc.dc.mutation.MutationConfiguration.ConfigKey;
import edu.mit.csail.sdg.parser.CompModule;
import edu.mit.csail.sdg.parser.CompUtil;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;

public class TestingMain {


    public static void main(String[] args) {
        MutationConfiguration mconfig = MutationConfiguration.getInstance();
        mconfig.setConfig(ConfigKey.OPERATOR_BES_STRICT_TYPE_CHECK, Boolean.TRUE);
        mconfig.setConfig(ConfigKey.OPERATOR_JEX_STRICT_TYPE_CHECK, Boolean.FALSE);

        if (args.length != 1)
            throw new IllegalArgumentException("Only one argument expected");
        if (args[0].isEmpty())
            throw new IllegalArgumentException("File name is empty");
        if (!Files.isRegularFile(Paths.get(args[0])))
            throw new IllegalArgumentException("File doesn't exist");

        String pathToFile = args[0];

        CompModule mainModule = CompUtil.parseEverything_fromFile(null, null, pathToFile);

        generateMutants(mainModule, Ops.JER, Ops.JEE, Ops.JES);

        //        for (Sig s : mainModule.getAllSigs()) {
        //            System.out.println(s.toExtendedString());
        //        }


    }

    private static void generateMutants(CompModule m, Ops... ops) {
//        m.updateMarkedExprsToMutate();

        List<Mutation> mutations = new LinkedList<>();
//        Optional<List<Expr>> expressions = m.();
//
//        if (!expressions.isPresent())
//            System.out.println("No expressions to mutate");
//
//        for (Expr e : m.getExpressionsToMutate().get()) {
//            for (Ops o : ops) {
//                if (o.isImplemented()) {
//                    Optional<List<Mutation>> opMutations = o.getOperator(m).getMutations(e);
//                    if (opMutations.isPresent())
//                        mutations.addAll(opMutations.get());
//                }
//            }
//        }

        System.out.println(mutations);
    }

}
