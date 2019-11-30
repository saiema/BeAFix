package ar.edu.unrc.dc.mutation;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class MutationConfiguration {

    public static enum ConfigKey {

                                  OPERATOR_BES_TYPE_CHECK

                                  {

                                      @Override
                                      public Class< ? > getValueType() {
                                          return Boolean.class;
                                      }
                                  };

        public abstract Class< ? > getValueType();
    }

    private static MutationConfiguration instance;
    private Map<String,Object>           config;

    public static MutationConfiguration getInstance() {
        if (instance == null)
            instance = new MutationConfiguration();
        return instance;
    }

    private MutationConfiguration() {
        config = new HashMap<>();
    }

    public void setConfig(ConfigKey configKey, Object value) {
        if (!configKey.getValueType().isAssignableFrom(value.getClass()))
            throw new IllegalArgumentException("Wrong value type for configuration key " + configKey.toString() + ", expected type is " + configKey.getValueType().getCanonicalName() + ", but got value of type " + value.getClass().getCanonicalName());
        this.config.put(configKey.toString(), value);
    }

    public Optional<Object> getConfigValue(ConfigKey configKey) {
        if (this.config.containsKey(configKey.toString())) {
            return Optional.of(this.config.get(configKey.toString()));
        }
        return Optional.empty();
    }

}
