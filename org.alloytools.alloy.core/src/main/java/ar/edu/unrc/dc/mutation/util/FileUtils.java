package ar.edu.unrc.dc.mutation.util;

import ar.edu.unrc.dc.mutation.MutationConfiguration;
import ar.edu.unrc.dc.mutation.mutantLab.Candidate;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestGenerationResult;
import ar.edu.unrc.dc.mutation.mutantLab.testGeneration.TestsGenerator;
import ar.edu.unrc.dc.mutation.visitors.ExprToStringNicePrint;
import edu.mit.csail.sdg.alloy4.Triplet;
import edu.mit.csail.sdg.ast.Command;
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

    public static void writeTests(File testsFile, CompModule world) {
        FileWriter myWriter;
        try {
            myWriter = new FileWriter(testsFile);
        } catch (IOException e) {
            throw new Error("Error occurred while creating FileWriter for tests", e);
        }
        for (Command c : world.getAllCommands()) {
            if (c.isGenerated()) {
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

    public static File[] setUpTestGenerationFiles(String originalFilename) {
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
        File testsFile = Paths.get(outputFolderPath, modelName + ".tests").toFile();
        File reportFile = Paths.get(outputFolderPath, modelName + ".report").toFile();
        if (testsFile.exists())
            throw new IllegalStateException("Tests file already exists ( " + testsFile.toString() + " )");
        try {
            testsFile.createNewFile();
        } catch (IOException e) {
            throw new Error("Couldn't create tests file ( " + testsFile.toString() + " )", e);
        }
        if (reportFile.exists())
            throw new IllegalStateException("Report file already exists ( " + reportFile.toString() + " )");
        try {
            reportFile.createNewFile();
        } catch (IOException e) {
            throw new Error("Couldn't create report file ( " + reportFile.toString() + " )", e);
        }
        return new File[] {testsFile, reportFile};
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
            FileWriter myWriter = new FileWriter(repairFile);;
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
