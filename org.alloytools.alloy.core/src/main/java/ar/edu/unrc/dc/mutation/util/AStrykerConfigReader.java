package ar.edu.unrc.dc.mutation.util;

import java.io.*;
import java.util.Properties;

public class AStrykerConfigReader {

    public enum Config_key {
        VARIABILIZATION {
            @Override
            public String getKey() {
                return "beafix.repair.variabilization";
            }
        },
        VARIABILIZATION_TEST_GENERATION {
            @Override
            public String getKey() {
                return "beafix.repair.variabilization.testgeneration";
            }
        },
        TEST_GENERATION_MAX_TESTS_PER_COMMAND {
            @Override
            public String getKey() {
                return "beafix.testgeneration.maxtestspercommand";
            }
        },
        TEST_GENERATION_TESTS_PER_STEP {
            @Override
            public String getKey() {
                return "beafix.testgeneration.testsperstep";
            }
        },
        TEST_GENERATION_AREPAIR_INTEGRATION {
            @Override
            public String getKey() {
                return "beafix.testgeneration.arepairintegration";
            }
        },
        TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_MODE {
            @Override
            public String getKey() { return "beafix.testgeneration.arepairintegration.relaxed"; }
        },
        TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_FACTS {
            @Override
            public String getKey() { return "beafix.testgeneration.arepairintegration.relaxedfacts"; }
        },
        TEST_GENERATION_AREPAIR_INTEGRATION_FORCE_ASSERTION_TESTS {
            @Override
            public String getKey() { return "beafix.testgeneration.arepairintegration.forceassertiontests"; }
        },
        TEST_GENERATION_NO_EXPECT_INSTANCE_FOR_NEGATION_TEST_WHEN_NO_FACTS {
            @Override
            public String getKey() { return "beafix.testgeneration.arepairintegration.noinstancetestfornegationtestwithnofacts"; }
        },
        TEST_GENERATION_AREPAIR_INSTANCE_TESTS_BRANCHES {
            @Override
            public String getKey() { return "beafix.testgeneration.arepairintegration.instancebranches"; }
        },
        TEST_GENERATION_NAME {
            @Override
            public String getKey() {
                return "beafix.testgeneration.testname";
            }
        },
        TEST_GENERATION_NAME_STARTING_INDEX {
            @Override
            public String getKey() {
                return "beafix.testgeneration.testname.startingindex";
            }
        },
        TEST_GENERATION_USE_MODEL_OVERRIDING {
            @Override
            public String getKey() {
                return "beafix.testgeneration.modeloverriding";
            }
        },
        TEST_GENERATION_MODEL_OVERRIDING_FOLDER {
            @Override
            public String getKey() {
                return "beafix.testgeneration.modeloverriding.overridingfolder";
            }
        },
        TEST_GENERATION_INSTANCES_TESTS_GENERATION {
            @Override
            public String getKey() {
                return "beafix.testgeneration.testsfrominstances";
            }
        },
        TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE {
            @Override
            public String getKey() {
                return "beafix.testgeneration.testsfrominstances.buggyfuncsfile";
            }
        },
        VARIABILIZATION_SAME_TYPE {
            @Override
            public String getKey() {
                return "beafix.repair.variabilization.sametype";
            }
        },
        PARTIAL_REPAIR {
            @Override
            public String getKey() {
                return "beafix.repair.partialrepair";
            }
        },
        PARTIAL_REPAIR_PRUNING {
            @Override
            public String getKey() {
                return "beafix.repair.partialrepair.pruning";
            }
        },
        PARTIAL_REPAIR_FULLCGRAPH_VALIDATION {
            @Override
            public String getKey() { return "beafix.repair.partialrepair.fullcgraphvalidation"; }
        },
        PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL {
            @Override
            public String getKey() { return "beafix.repair.partialrepair.independenttestsforall"; }
        },
        USE_PO_TO_VALIDATE {
            @Override
            public String getKey() {
                return "beafix.repair.validatewithpo";
            }
        },
        TIMEOUT {
            @Override
            public String getKey() {
                return "beafix.repair.timeout";
            }
        },
        MAX_DEPTH {
            @Override
            public String getKey() {
                return "beafix.repair.maxdepth";
            }
        },

        TEST_GENERATION_OUTPUT_FOLDER {
            @Override
            public String getKey() {
                return "beafix.testgeneration.outputfolder";
            }
        },

        TEST_GENERATION_OUTPUT_TO_FILES {
            @Override
            public String getKey() {
                return "beafix.testgeneration.outputtofiles";
            }
        },

        MUTANTS_GENERATION_OUTPUT_FOLDER {
            @Override
            public String getKey() {
                return "beafix.mutantgeneration.outputfolder";
            }
        },

        MUTANTS_GENERATION_CHECK {
            @Override
            public String getKey() {
                return "beafix.mutantgeneration.check";
            }
        },

        MUTANTS_GENERATION_LIMIT {
            @Override
            public String getKey() {
                return "beafix.mutantgeneration.limit";
            }
        },

        HACKS_CANDIDATE_HASHES {
            @Override
            public String getKey() {
                return "beafix.hacks.candidatehashes";
            }
        }

        ;
        public abstract String getKey();

    }

    /**
     * The path to a default .properties file
     */
    private static final String DEFAULT_PROPERTIES = "beafix.properties";

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
        prop.store(outputStream, "BEAFIX AUTOGENERATED PROPERTIES - DO NOT MODIFY");
    }

    public void removeConfig(Config_key key) {
        prop.remove(key.getKey());
    }

    public boolean argumentExist(Config_key key) {
        return prop.get(key.getKey()) != null;
    }

    public boolean getBooleanArgument(Config_key key) {
        if (isNotBooleanKey(key))
            throw new IllegalStateException("Config key is not boolean " + key);
        if (isNotDefined(key))
            return false;
        String propValue = prop.getProperty(key.getKey());
        if (propValue == null)
            return false;
        return Boolean.parseBoolean(propValue);
    }

    public int getIntArgument(Config_key key) {
        if (isNotIntKey(key))
            throw new IllegalStateException("Config key is not int " + key);
        if (isNotDefined(key))
            return 0;
        String propValue = prop.getProperty(key.getKey());
        if (propValue == null)
            return 0;
        return Integer.parseInt(propValue);
    }

    public String getStringArgument(Config_key key) {
        if (isNotStringKey(key))
            throw new IllegalStateException("Config key is not String " + key);
        if (isNotDefined(key))
            return "";
        return prop.getProperty(key.getKey(), "");
    }

    public void setIntArgument(Config_key key, int value) {
        if (isNotIntKey(key))
            throw new IllegalStateException("Config key is not int " + key);
        prop.setProperty(key.getKey(), Integer.toString(value));
    }

    public void setBooleanArgument(Config_key key, boolean value) {
        if (isNotBooleanKey(key))
            throw new IllegalStateException("Config key is not boolean " + key);
        prop.setProperty(key.getKey(), Boolean.toString(value));
    }

    public void setStringArgument(Config_key key, String value) {
        if (isNotStringKey(key))
            throw new IllegalStateException("Config key is not String " + key);
        prop.setProperty(key.getKey(), value);
    }

    private boolean isNotDefined(Config_key key) {
        return !prop.containsKey(key.getKey());
    }


    private boolean isNotBooleanKey(Config_key key) {
        switch (key) {
            case VARIABILIZATION:
            case VARIABILIZATION_SAME_TYPE:
            case VARIABILIZATION_TEST_GENERATION:
            case USE_PO_TO_VALIDATE:
            case PARTIAL_REPAIR_FULLCGRAPH_VALIDATION:
            case PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL:
            case TEST_GENERATION_OUTPUT_TO_FILES:
            case TEST_GENERATION_AREPAIR_INTEGRATION:
            case TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_MODE:
            case TEST_GENERATION_AREPAIR_INTEGRATION_RELAXED_FACTS:
            case TEST_GENERATION_AREPAIR_INTEGRATION_FORCE_ASSERTION_TESTS:
            case TEST_GENERATION_NO_EXPECT_INSTANCE_FOR_NEGATION_TEST_WHEN_NO_FACTS:
            case TEST_GENERATION_USE_MODEL_OVERRIDING:
            case TEST_GENERATION_INSTANCES_TESTS_GENERATION:
            case MUTANTS_GENERATION_CHECK:
            case PARTIAL_REPAIR_PRUNING:
            case PARTIAL_REPAIR: return false;
            default : return true;
        }
    }

    private boolean isNotIntKey(Config_key key) {
        switch (key) {
            case TIMEOUT    :
            case TEST_GENERATION_MAX_TESTS_PER_COMMAND:
            case TEST_GENERATION_TESTS_PER_STEP:
            case TEST_GENERATION_NAME_STARTING_INDEX:
            case MUTANTS_GENERATION_LIMIT:
            case MAX_DEPTH  :  return false;
            default : return true;
        }
    }

    private boolean isNotStringKey(Config_key key) {
        switch (key) {
            case MUTANTS_GENERATION_OUTPUT_FOLDER:
            case TEST_GENERATION_NAME:
            case TEST_GENERATION_MODEL_OVERRIDING_FOLDER:
            case TEST_GENERATION_INSTANCES_TESTS_GENERATION_BUGGY_FUNCS_FILE:
            case TEST_GENERATION_OUTPUT_FOLDER:
            case TEST_GENERATION_AREPAIR_INSTANCE_TESTS_BRANCHES:
            case HACKS_CANDIDATE_HASHES: return false;
            default : return true;
        }
    }


}
