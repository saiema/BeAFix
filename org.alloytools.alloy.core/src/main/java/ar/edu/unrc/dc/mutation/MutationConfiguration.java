package ar.edu.unrc.dc.mutation;

import ar.edu.unrc.dc.mutation.util.AStrykerConfigReader;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class MutationConfiguration {

    public enum ConfigKey {

        OPERATOR_BES_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }
        },

        OPERATOR_JEX_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }
        },

        OPERATOR_VCR_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }
        },

        OPERATOR_QTBER_BOUND_MAX_GENERATION {
            @Override
            public Class<?> getValueType() {
                return Integer.class;
            }

            @Override
            public Object defaultValue() {
                return 3;
            }
        },

        OPERATOR_QTBER_BOUND_MIN_GENERATION {
            @Override
            public Class<?> getValueType() {
                return Integer.class;
            }

            @Override
            public Object defaultValue() {
                return 1;
            }
        },

        OPERATOR_SSX_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }
        },

        OPERATOR_EMOR_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        OPERATOR_RUOD_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        OPERATOR_BIN_OP_REPLACEMENT_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        OPERATOR_QTOI_STRICT_TYPE_CHECK {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        MUTATION_STRICT_TYPE_CHECKING {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        MUTATION_BOUND_MUTATION_BY_ANY_OPERATOR {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        MUTATION_TOSTRING_FULL {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        REPAIR_USE_DEPENDENCY_GRAPH_FOR_CHECKING {
            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        REPAIR_MAX_DEPTH {
            @Override
            public Class<?> getValueType() {
                return Integer.class;
            }

            @Override
            public Object defaultValue() {
                return Integer.MAX_VALUE;
            }

        },

        REPAIR_GENERATOR_TRIGGER_THRESHOLD {
            @Override
            public Class<?> getValueType() {
                return Integer.class;
            }

            @Override
            public Object defaultValue() {
                return 5;
            }

        },

        REPAIR_DEBUG_SKIP_VERIFICATION {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        REPAIR_GENERATOR_CANDIDATE_GETTER_TIMEOUT {
            @Override
            public Class<?> getValueType() {
                return Long.class;
            }

            @Override
            public Object defaultValue() {
                return 5000L;
            }

        },

        REPAIR_VARIABILIZATION {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        REPAIR_VARIABILIZATION_TEST_GENERATION {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        TEST_GENERATION_TESTS_PER_STEP {

            @Override
            public Class<?> getValueType() {
                return Integer.class;
            }

            @Override
            public Object defaultValue() {
                return 1;
            }

        },

        TEST_GENERATION_MAX_TESTS_PER_COMMAND {

            @Override
            public Class<?> getValueType() {
                return Integer.class;
            }

            @Override
            public Object defaultValue() {
                return 4;
            }

        },

        REPAIR_TESTS_ONLY {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        REPAIR_VARIABILIZATION_USE_SAME_TYPES {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        REPAIR_PARTIAL_REPAIR {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        REPAIR_PARTIAL_REPAIR_FULL_CALLGRAPH_VALIDATION {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        REPAIR_PARTIAL_REPAIR_REQUIRE_TESTS_FOR_ALL {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        REPAIR_TIMEOUT {

            @Override
            public Class<?> getValueType() {
                return Long.class;
            }

            @Override
            public Object defaultValue() {
                return 0L;
            }

        },

        TEST_GENERATION_OUTPUT_FOLDER {

            @Override
            public Class<?> getValueType() {
                return String.class;
            }

            @Override
            public Object defaultValue() {
                return "";
            }

        },

        TEST_GENERATION_OUTPUT_TO_FILES {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.FALSE;
            }

        },

        MUTANT_GENERATION_OUTPUT_FOLDER {

            @Override
            public Class<?> getValueType() {
                return String.class;
            }

            @Override
            public Object defaultValue() {
                return "";
            }

        },

        MUTANT_GENERATION_CHECK {

            @Override
            public Class<?> getValueType() {
                return Boolean.class;
            }

            @Override
            public Object defaultValue() {
                return Boolean.TRUE;
            }

        },

        MUTANT_GENERATION_LIMIT {

            @Override
            public Class<?> getValueType() {
                return Integer.class;
            }

            @Override
            public Object defaultValue() {
                return 0;
            }

        }

        ;

        public abstract Class<?> getValueType();

        public abstract Object defaultValue();
    }

    private static MutationConfiguration instance;
    private final Map<String, Object> config;

    public static MutationConfiguration getInstance() {
        if (instance == null)
            instance = new MutationConfiguration();
        return instance;
    }

    private MutationConfiguration() {
        config = new HashMap<>();
    }

    public void loadConfigFromAStrykerConfig() {
        AStrykerConfigReader aconfig = AStrykerConfigReader.getInstance();
        try {
            aconfig.loadConfig();
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.VARIABILIZATION)) {
                setConfig(ConfigKey.REPAIR_VARIABILIZATION, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.VARIABILIZATION));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.VARIABILIZATION_TEST_GENERATION)) {
                setConfig(ConfigKey.REPAIR_VARIABILIZATION_TEST_GENERATION, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.VARIABILIZATION_TEST_GENERATION));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.VARIABILIZATION_SAME_TYPE)) {
                setConfig(ConfigKey.REPAIR_VARIABILIZATION_USE_SAME_TYPES, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.VARIABILIZATION_SAME_TYPE));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.PARTIAL_REPAIR)) {
                setConfig(ConfigKey.REPAIR_PARTIAL_REPAIR, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_FULLCGRAPH_VALIDATION)) {
                setConfig(ConfigKey.REPAIR_PARTIAL_REPAIR_FULL_CALLGRAPH_VALIDATION, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_FULLCGRAPH_VALIDATION));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL)) {
                setConfig(ConfigKey.REPAIR_PARTIAL_REPAIR_REQUIRE_TESTS_FOR_ALL, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.PARTIAL_REPAIR_INDEPENDENT_TESTS_FOR_ALL));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.USE_PO_TO_VALIDATE)) {
                setConfig(ConfigKey.REPAIR_TESTS_ONLY, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.USE_PO_TO_VALIDATE));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.TIMEOUT)) {
                int to = aconfig.getIntArgument(AStrykerConfigReader.Config_key.TIMEOUT);
                Long toAsLong = to == 0?0L:(to * 1000) * 60;
                setConfig(ConfigKey.REPAIR_TIMEOUT, toAsLong);
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.MAX_DEPTH)) {
                setConfig(ConfigKey.REPAIR_MAX_DEPTH, aconfig.getIntArgument(AStrykerConfigReader.Config_key.MAX_DEPTH));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.TEST_GENERATION_MAX_TESTS_PER_COMMAND)) {
                setConfig(ConfigKey.TEST_GENERATION_MAX_TESTS_PER_COMMAND, aconfig.getIntArgument(AStrykerConfigReader.Config_key.TEST_GENERATION_MAX_TESTS_PER_COMMAND));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.TEST_GENERATION_TESTS_PER_STEP)) {
                setConfig(ConfigKey.TEST_GENERATION_TESTS_PER_STEP, aconfig.getIntArgument(AStrykerConfigReader.Config_key.TEST_GENERATION_TESTS_PER_STEP));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.TEST_GENERATION_OUTPUT_FOLDER)) {
                setConfig(ConfigKey.TEST_GENERATION_OUTPUT_FOLDER, aconfig.getStringArgument(AStrykerConfigReader.Config_key.TEST_GENERATION_OUTPUT_FOLDER));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.TEST_GENERATION_OUTPUT_TO_FILES)) {
                setConfig(ConfigKey.TEST_GENERATION_OUTPUT_TO_FILES, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.TEST_GENERATION_OUTPUT_TO_FILES));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.MUTANTS_GENERATION_OUTPUT_FOLDER)) {
                setConfig(ConfigKey.MUTANT_GENERATION_OUTPUT_FOLDER, aconfig.getStringArgument(AStrykerConfigReader.Config_key.MUTANTS_GENERATION_OUTPUT_FOLDER));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.MUTANTS_GENERATION_CHECK)) {
                setConfig(ConfigKey.MUTANT_GENERATION_CHECK, aconfig.getBooleanArgument(AStrykerConfigReader.Config_key.MUTANTS_GENERATION_CHECK));
            }
            if (aconfig.argumentExist(AStrykerConfigReader.Config_key.MUTANTS_GENERATION_LIMIT)) {
                setConfig(ConfigKey.MUTANT_GENERATION_LIMIT, aconfig.getIntArgument(AStrykerConfigReader.Config_key.MUTANTS_GENERATION_LIMIT));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void setConfig(ConfigKey configKey, Object value) {
        if (!configKey.getValueType().isAssignableFrom(value.getClass()))
            throw new IllegalArgumentException("Wrong value type for configuration key " + configKey.toString() + ", expected type is " + configKey.getValueType().getCanonicalName() + ", but got value of type " + value.getClass().getCanonicalName());
        if (configKey.equals(ConfigKey.MUTATION_STRICT_TYPE_CHECKING)) {
            this.config.put(configKey.toString(), value);
            setConfig(ConfigKey.OPERATOR_BES_STRICT_TYPE_CHECK, value);
            setConfig(ConfigKey.OPERATOR_BIN_OP_REPLACEMENT_STRICT_TYPE_CHECK, value);
            setConfig(ConfigKey.OPERATOR_EMOR_STRICT_TYPE_CHECK, value);
            setConfig(ConfigKey.OPERATOR_JEX_STRICT_TYPE_CHECK, value);
            setConfig(ConfigKey.OPERATOR_VCR_STRICT_TYPE_CHECK, value);
            setConfig(ConfigKey.OPERATOR_SSX_STRICT_TYPE_CHECK, value);
            setConfig(ConfigKey.OPERATOR_QTOI_STRICT_TYPE_CHECK, value);
            setConfig(ConfigKey.OPERATOR_RUOD_STRICT_TYPE_CHECK, value);
        } else
            this.config.put(configKey.toString(), value);
    }

    public Optional<Object> getConfigValue(ConfigKey configKey) {
        if (this.config.containsKey(configKey.toString())) {
            return Optional.of(this.config.get(configKey.toString()));
        }
        return Optional.empty();
    }

    public Object getConfigValueOrDefault(ConfigKey configKey) {
        Optional<Object> resOp = getConfigValue(configKey);
        return resOp.orElseGet(configKey::defaultValue);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("***MUTATION CONFIGURATION***\n");
        for (ConfigKey configKey : ConfigKey.values()) {
            Optional<Object> configValue = getConfigValue(configKey);
            sb.append(configKey.toString()).append(" : ");
            if (configValue.isPresent()) {
                sb.append(configValue.get().toString());
            } else {
                sb.append(configKey.defaultValue().toString());
            }
            sb.append("\n");
        }
        return sb.toString();
    }

}
