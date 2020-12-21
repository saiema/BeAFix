package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestGenerationResult;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestsGenerator;
import ar.edu.unrc.dc.mutation.visitors.ExprToStringNicePrint;
import edu.mit.csail.sdg.alloy4.Triplet;
import edu.mit.csail.sdg.ast.Command;
import edu.mit.csail.sdg.ast.Command.TestType;
import edu.mit.csail.sdg.ast.ExprHasName;
import edu.mit.csail.sdg.ast.Func;
import edu.mit.csail.sdg.parser.CompModule;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;

public class FileUtils {

    private static Logger logger;
    public static void setLogger(Logger logger) {
        FileUtils.logger = logger;
    }

    public static File writeCandidateToFile(Candidate candidate, String fileName, boolean isRepair, boolean writeMutations) {
        try {
            String outputFile = isRepair?fileName.replace(".als", "_repair.als"):fileName;
            File repairFile = new File(outputFile);
            if (repairFile.exists()) {
                if (!repairFile.delete()) {
                    logger.info("Failed to write repair file : " + repairFile.toString());
                    return null;
                }
            }
            CandidateWriter candidateWriter = new CandidateWriter(candidate);
            FileWriter myWriter = new FileWriter(repairFile);
            if (writeMutations) {
                for (Triplet<String,String,String> mutation : candidate.getCurrentMutationsInfo()) {
                    myWriter.write("//Line and OP: " + mutation.a + "\n//ORIGINAL: " + mutation.b + "\n//MUTATION: " + mutation.c + "\n");
                }
            }
            myWriter.write(candidateWriter.candidateStringRepresentation());
            myWriter.close();
            return repairFile;
        } catch (Exception e) {
            StringWriter sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            logger.info("Error occurred while writing repair file\n" + exceptionAsString);
            return null;
        }
    }

    public static void writeTests(File testsFile, CompModule world, TestType testType) {
        FileWriter myWriter;
        try {
            myWriter = new FileWriter(testsFile);
        } catch (IOException e) {
            throw new Error("Error occurred while creating FileWriter for tests", e);
        }
        for (Command c : world.getAllCommands()) {
            if (c.isGenerated()) {
                if (!c.testType().equals(testType))
                    continue;
                String command = c.toString();
                Optional<Func> testFunc = DependencyScanner.getFuncByName(((ExprHasName)c.nameExpr).label, world.getAllFunc());
                if (!testFunc.isPresent())
                    throw new Error("Something went wrong, test command " + command + " has no associated predicate");
                ExprToStringNicePrint toString = new ExprToStringNicePrint(null);
                toString.visitPredicate(testFunc.get());
                String predicate = toString.getStringRepresentation();
                try {
                    myWriter.write(predicate);
                    myWriter.write("\n\n");
                    myWriter.write(command);
                    myWriter.write("\n\n");
                    myWriter.write("===TEST===\n");
                } catch (IOException e) {
                    throw new Error("An error occurred while trying to write generated test", e);
                }
            }
        }
        try {
            myWriter.close();
        } catch (IOException e) {
            throw new Error("An error occurred while closing tests file", e);
        }
    }

    public static void writeReport(File reportFile, TestGenerationResult lastTestGenerationRes) {
        FileWriter myWriter;
        try {
            myWriter = new FileWriter(reportFile);
        } catch (IOException e) {
            throw new Error("Error occurred while creating FileWriter for report", e);
        }
        switch (lastTestGenerationRes) {
            case NO_INSTANCES_GENERATED:
            case NO_RUN_TESTS_TO_RUN:
            case UNDEFINED:
            case NO_TESTS_TO_RUN:
            case NO_CHECK_TESTS_TO_RUN:
            case NO_FAILING_TEST: {
                try {
                    myWriter.write(lastTestGenerationRes.toString());
                    myWriter.write("\n");
                } catch (IOException e) {
                    throw new Error("An error occurred while trying to write report", e);
                }
                break;
            }
            case GENERATED: {
                StringBuilder sb = new StringBuilder("Generated tests per command\n");
                for (Map.Entry<String, Integer> propertyTestAmount : TestsGenerator.getInstance().getTestAmountPerProperty().entrySet()) {
                    sb.append(propertyTestAmount.getKey()).append(" : ").append(propertyTestAmount.getValue()).append("\n");
                }
                try {
                    myWriter.write(sb.toString());
                } catch (IOException e) {
                    throw new Error("An error occurred while trying to write report", e);
                }
                break;
            }
        }
        try {
            myWriter.close();
        } catch (IOException e) {
            throw new Error("An error occurred while closing report file", e);
        }
    }

    private static final String CE_FILE_POSTFIX = "_counterexamples.tests";
    private static final String POS_TRUSTED_POSTFIX = "_positive_trusted.tests";
    private static final String POS_UNTRUSTED_POSTFIX = "_positive_untrusted.tests";
    private static final String NEG_TRUSTED_POSTFIX = "_negative_trusted.tests";
    private static final String NEG_UNTRUSTED_POSTFIX = "_negative_untrusted.tests";
    public static File[] setUpTestGenerationFiles(String originalFilename, boolean includePosAndNegTests) {
        String outputFolderPath = (String) MutationConfiguration.getInstance().getConfigValue(MutationConfiguration.ConfigKey.TEST_GENERATION_OUTPUT_FOLDER).orElse("");
        File outFolder = new File(outputFolderPath);
        if (!outFolder.exists())
            throw new IllegalStateException("tests output folder doesn't exists ( " + outputFolderPath + ")");
        if (!outFolder.isDirectory())
            throw new IllegalStateException("tests output folder is not a folder ( " + outputFolderPath + ")");
        if (!outFolder.canExecute() || !outFolder.canWrite())
            throw new IllegalStateException("Insufficient access to output folder ( " + outputFolderPath + ")");
        Path modelFileAsPath = Paths.get(originalFilename);
        String modelName = modelFileAsPath.getFileName().toString().replace(".als", "");
        File testsFile = Paths.get(outputFolderPath, modelName + CE_FILE_POSTFIX).toFile();
        File reportFile = Paths.get(outputFolderPath, modelName + CE_FILE_POSTFIX + ".report").toFile();
        createNewFile(testsFile);
        createNewFile(reportFile);
        if (includePosAndNegTests) {
            File positiveTestsTrusted = Paths.get(outputFolderPath, modelName + POS_TRUSTED_POSTFIX).toFile();
            File positiveTestsUntrusted = Paths.get(outputFolderPath, modelName + POS_UNTRUSTED_POSTFIX).toFile();
            File negativeTrustedTests = Paths.get(outputFolderPath, modelName + NEG_TRUSTED_POSTFIX).toFile();
            File negativeUntrustedTests = Paths.get(outputFolderPath, modelName + NEG_UNTRUSTED_POSTFIX).toFile();
            createNewFile(positiveTestsTrusted);
            createNewFile(positiveTestsUntrusted);
            createNewFile(negativeTrustedTests);
            createNewFile(negativeUntrustedTests);
            return new File[] {testsFile, reportFile, positiveTestsTrusted, positiveTestsUntrusted, negativeTrustedTests, negativeUntrustedTests};
        }
        return new File[] {testsFile, reportFile};
    }

    private static void createNewFile(File f) {
        try {
            if (!f.createNewFile())
                throw new IllegalStateException("File already exists ( " + f.toString() + " )");
        } catch (IOException e) {
            throw new Error("Couldn't create file ( " + f.toString() + " )", e);
        }
    }

    public static void writeCheckReportToFile(String originalFileName, String result) {
        try {
            File repairFile = new File(originalFileName.replace(".als", ".verification"));
            if (repairFile.exists()) {
                if (!repairFile.delete()) {
                    logger.info("Failed to write verification file : " + repairFile.toString());
                    return;
                }
            }
            FileWriter myWriter = new FileWriter(repairFile);
            myWriter.write(result);
            myWriter.close();
        } catch (Exception e) {
            StringWriter sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            String exceptionAsString = sw.toString();
            logger.info("Error occurred while writing verification file\n" + exceptionAsString);
        }
    }

}
