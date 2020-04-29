package ar.edu.unrc.dc.mutation;

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

        REPAIR_TIMEOUT {

            @Override
            public Class<?> getValueType() {
                return Long.class;
            }

            @Override
            public Object defaultValue() {
                return 0L;
            }

        }

        ;

        public abstract Class<?> getValueType();

        public abstract Object defaultValue();
    }

    private static MutationConfiguration instance;
    private Map<String, Object> config;

    public static MutationConfiguration getInstance() {
        if (instance == null)
            instance = new MutationConfiguration();
        return instance;
    }

    private MutationConfiguration() {
        config = new HashMap<>();
        //loadSystemProperties();
    }

    public void loadSystemProperties() {
        for (ConfigKey ck : ConfigKey.values()) {
            if (ck.equals(ConfigKey.REPAIR_VARIABILIZATION))
                continue;
            if (ck.equals(ConfigKey.REPAIR_TIMEOUT))
                continue;
            if (ck.equals(ConfigKey.REPAIR_MAX_DEPTH))
                continue;
            if (ck.equals(ConfigKey.REPAIR_PARTIAL_REPAIR))
                continue;
            String propValue = System.getenv(ck.toString());
            if (propValue != null) {
                if (ck.getValueType().equals(Boolean.class)) {
                    Boolean configValue = Boolean.valueOf(propValue);
                    setConfig(ck, configValue);
                } else if (ck.getValueType().equals(Integer.class)) {
                    try {
                        Integer configValue = Integer.parseInt(propValue);
                        setConfig(ck, configValue);
                    } catch (NumberFormatException e) {
                        System.err.println("Error while parsing value for property " + ck.toString());
                    }
                } else if (ck.getValueType().equals(Long.class)) {
                    try {
                        Long configValue = Long.parseLong(propValue);
                        setConfig(ck, configValue);
                    } catch (NumberFormatException e) {
                        System.err.println("Error while parsing value for property " + ck.toString());
                    }
                }
            }
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
