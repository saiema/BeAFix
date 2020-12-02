package ar.edu.unrc.dc.mutation.util;

import java.io.*;
import java.util.Properties;

public class AStrykerConfigReader {

    public enum Config_key {
        VARIABILIZATION {
            @Override
            public String getKey() {
                return "astryker.repair.variabilization";
            }
        },
        VARIABILIZATION_TEST_GENERATION {
            @Override
            public String getKey() {
                return "astryker.repair.variabilization.testgeneration";
            }
        },
        TEST_GENERATION_MAX_TESTS_PER_COMMAND {
            @Override
            public String getKey() {
                return "astryker.testgeneration.maxtestspercommand";
            }
        },
        TEST_GENERATION_TESTS_PER_STEP {
            @Override
            public String getKey() {
                return "astryker.testgeneration.testsperstep";
            }
        },
        TEST_GENERATION_AREPAIR_INTEGRATION {
            @Override
            public String getKey() {
                return "astryker.testgeneration.arepairintegration";
            }
        },
        TEST_GENERATION_NAME {
            @Override
            public String getKey() {
                return "astryker.testgeneration.testname";
            }
        },
        TEST_GENERATION_NAME_STARTING_INDEX {
            @Override
            public String getKey() {
                return "astryker.testgeneration.testname.startingindex";
            }
        },
        TEST_GENERATION_USE_MODEL_OVERRIDING {
            @Override
            public String getKey() {
                return "astryker.testgeneration.modeloverriding";
            }
        },
        TEST_GENERATION_MODEL_OVERRIDING_FOLDER {
            @Override
            public String getKey() {
                return "astryker.testgeneration.modeloverriding.overridingfolder";
            }
        },
        TEST_GENERATION_INSTANCES_TESTS_GENERATION {
            @Override
            public String getKey() {
                return "astryker.testgeneration.testsfrominstances";
            }
        },
        TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE {
            @Override
            public String getKey() {
                return "astryker.testgeneration.testsfrominstances.buggyfuncsfile";
            }
        },
        VARIABILIZATION_SAME_TYPE {
            @Override
            public String getKey() {
                return "astryker.repair.variabilization.sametype";
            }
        },
        PARTIAL_REPAIR {
            @Override
            public String getKey() {
                return "astryker.repair.partialrepair";
            }
        },
        PARTIAL_REPAIR_PRUNING {
            @Override
            public String getKey() {
                return "astryker.repair.partialrepair.pruning";
            }
        },
        PARTIAL_REPAIR_FULLCGRAPH_VALIDATION {
            @Override
            public String getKey() { return "astryker.repair.partialrepair.fullcgraphvalidation"; }
        },
        PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL {
            @Override
            public String getKey() { return "astryker.repair.partialrepair.independenttestsforall"; }
        },
        USE_PO_TO_VALIDATE {
            @Override
            public String getKey() {
                return "astryker.repair.validatewithpo";
            }
        },
        TIMEOUT {
            @Override
            public String getKey() {
                return "astryker.repair.timeout";
            }
        },
        MAX_DEPTH {
            @Override
            public String getKey() {
                return "astryker.repair.maxdepth";
            }
        },

        TEST_GENERATION_OUTPUT_FOLDER {
            @Override
            public String getKey() {
                return "astryker.testgeneration.outputfolder";
            }
        },

        TEST_GENERATION_OUTPUT_TO_FILES {
            @Override
            public String getKey() {
                return "astryker.testgeneration.outputtofiles";
            }
        },

        MUTANTS_GENERATION_OUTPUT_FOLDER {
            @Override
            public String getKey() {
                return "astryker.mutantgeneration.outputfolder";
            }
        },

        MUTANTS_GENERATION_CHECK {
            @Override
            public String getKey() {
                return "astryker.mutantgeneration.check";
            }
        },

        MUTANTS_GENERATION_LIMIT {
            @Override
            public String getKey() {
                return "astryker.mutantgeneration.limit";
            }
        },

        HACKS_CANDIDATE_HASHES {
            @Override
            public String getKey() {
                return "astryker.hacks.candidatehashes";
            }
        }

        ;
        public abstract String getKey();

    }

    /**
     * The path to a default .properties file
     */
    private static final String DEFAULT_PROPERTIES = "astryker.properties";

    /**
     * The {@code StrykerConfig} instance that will be returned by {@link AStrykerConfigReader#getInstance()}
     */
    private static AStrykerConfigReader instance = null;

    /**
     * @return a previously built instance or construct a new instance using {@code StrykerConfig#DEFAULT_PROPERTIES}
     */
    public static AStrykerConfigReader getInstance() {
        if (instance == null) {
            try {
                instance = new AStrykerConfigReader();
            } catch (IOException e) {
                throw new IllegalStateException("Exception when trying to load properties");
            }
        }
        return instance;
    }

    private AStrykerConfigReader() throws IOException {
        prop = new Properties();
        loadPropertiesFromFile();
    }

    private void loadPropertiesFromFile() throws IOException {
        String cwd = System.getProperty("user.dir");
        String configFile = cwd + File.separator + DEFAULT_PROPERTIES;
        File propFile = createConfigFileIfMissing(configFile);
        InputStream inputStream = new FileInputStream(propFile);
        prop.load(inputStream);
    }

    private Properties prop;

    private File createConfigFileIfMissing(String configFile) throws IOException {
        File propFile = new File(configFile);
        if (!propFile.exists())
            if (!propFile.createNewFile())
                throw new IllegalStateException("Couldn't create new file " + configFile);
        return propFile;
    }

    public void loadConfig() throws IOException {
        loadPropertiesFromFile();
    }

    public void saveConfig() throws IOException {
        if (prop == null)
            prop = new Properties();
        String cwd = System.getProperty("user.dir");
        String configFile = cwd + File.separator + DEFAULT_PROPERTIES;
        File propFile = createConfigFileIfMissing(configFile);
        OutputStream outputStream = new FileOutputStream(propFile);
        prop.store(outputStream, "ASTRYKER AUTOGENERATED PROPERTIES - DO NOT MODIFY");
    }

    public void removeConfig(Config_key key) {
        prop.remove(key.getKey());
    }

    public boolean argumentExist(Config_key key) {
        return prop.get(key.getKey()) != null;
    }

    public boolean getBooleanArgument(Config_key key) {
        if (!isBooleanKey(key))
            throw new IllegalStateException("Config key is not boolean " + key.toString());
        if (!isDefined(key))
            return false;
        String propValue = prop.getProperty(key.getKey());
        if (propValue == null)
            return false;
        return Boolean.parseBoolean(propValue);
    }

    public int getIntArgument(Config_key key) {
        if (!isIntKey(key))
            throw new IllegalStateException("Config key is not int " + key.toString());
        if (!isDefined(key))
            return 0;
        String propValue = prop.getProperty(key.getKey());
        if (propValue == null)
            return 0;
        return Integer.parseInt(propValue);
    }

    public String getStringArgument(Config_key key) {
        if (!isStringKey(key))
            throw new IllegalStateException("Config key is not String " + key.toString());
        if (!isDefined(key))
            return "";
        return prop.getProperty(key.getKey(), "");
    }

    public void setIntArgument(Config_key key, int value) {
        if (!isIntKey(key))
            throw new IllegalStateException("Config key is not int " + key.toString());
        prop.setProperty(key.getKey(), Integer.toString(value));
    }

    public void setBooleanArgument(Config_key key, boolean value) {
        if (!isBooleanKey(key))
            throw new IllegalStateException("Config key is not boolean " + key.toString());
        prop.setProperty(key.getKey(), Boolean.toString(value));
    }

    public void setStringArgument(Config_key key, String value) {
        if (!isStringKey(key))
            throw new IllegalStateException("Config key is not String " + key.toString());
        prop.setProperty(key.getKey(), value);
    }

    private boolean isDefined(Config_key key) {
        return prop.containsKey(key.getKey());
    }


    private boolean isBooleanKey(Config_key key) {
        switch (key) {
            case VARIABILIZATION:
            case VARIABILIZATION_SAME_TYPE:
            case VARIABILIZATION_TEST_GENERATION:
            case USE_PO_TO_VALIDATE:
            case PARTIAL_REPAIR_FULLCGRAPH_VALIDATION:
            case PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL:
            case TEST_GENERATION_OUTPUT_TO_FILES:
            case TEST_GENERATION_AREPAIR_INTEGRATION:
            case TEST_GENERATION_USE_MODEL_OVERRIDING:
            case TEST_GENERATION_INSTANCES_TESTS_GENERATION:
            case MUTANTS_GENERATION_CHECK:
            case PARTIAL_REPAIR_PRUNING:
            case PARTIAL_REPAIR: return true;
            default : return false;
        }
    }

    private boolean isIntKey(Config_key key) {
        switch (key) {
            case TIMEOUT    :
            case TEST_GENERATION_MAX_TESTS_PER_COMMAND:
            case TEST_GENERATION_TESTS_PER_STEP:
            case TEST_GENERATION_NAME_STARTING_INDEX:
            case MUTANTS_GENERATION_LIMIT:
            case MAX_DEPTH  :  return true;
            default : return false;
        }
    }

    private boolean isStringKey(Config_key key) {
        switch (key) {
            case MUTANTS_GENERATION_OUTPUT_FOLDER:
            case TEST_GENERATION_NAME:
            case TEST_GENERATION_MODEL_OVERRIDING_FOLDER:
            case TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE:
            case TEST_GENERATION_OUTPUT_FOLDER:
            case HACKS_CANDIDATE_HASHES: return true;
            default : return false;
        }
    }


}
